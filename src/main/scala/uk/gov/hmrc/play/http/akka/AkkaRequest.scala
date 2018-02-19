/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.play.http.akka

import java.net.InetSocketAddress

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.model.{HttpHeader, HttpRequest}
import akka.http.scaladsl.settings.ConnectionPoolSettings
import akka.http.scaladsl.{ClientTransport, Http, HttpExt}
import com.typesafe.config.Config
import play.api.{Configuration, Play}
import uk.gov.hmrc.http.{HeaderCarrier, Request}

import scala.collection.immutable

trait AkkaRequest extends Request {

  override lazy val configuration: Option[Config] = Play.maybeApplication.map(_.configuration.underlying)

  def akkaClient: HttpExt = {
    Http()
  }

  def buildRequest[A](url: String)(implicit hc: HeaderCarrier): HttpRequest = {
    val headers = applicableHeaders(url)(hc)
      .map { case (key, value) => HttpHeader.parse(key, value) }
      .collect { case Ok(h, _) => h }
      .to[immutable.Seq]

    HttpRequest(uri = url).withHeaders(headers)
  }
}


trait AkkaProxy extends AkkaRequest {

  def system: ActorSystem

  def akkaProxyServer: Option[AkkaProxyServer]

  lazy val httpsProxyTransport: Option[ClientTransport] = akkaProxyServer map (
    proxy =>
      ClientTransport.httpsProxy(
        InetSocketAddress.createUnresolved(proxy.host, proxy.port),
        BasicHttpCredentials(proxy.principal.get, proxy.password.get))
    )

  lazy val settings: ConnectionPoolSettings = httpsProxyTransport
    .fold(ConnectionPoolSettings(system))(transport => ConnectionPoolSettings(system).withTransport(transport))

  override def buildRequest[A](url: String)(implicit hc: HeaderCarrier): HttpRequest = {
    akkaProxyServer match {
      case Some(proxy) => super.buildRequest(url)
      case None => super.buildRequest(url)
    }
  }
}

object AkkaProxyConfiguration {

  def apply(configPrefix: String, configuration: Configuration): Option[AkkaProxyServer] = {
    val proxyRequired = configuration.getBoolean(s"$configPrefix.proxyRequiredForThisEnvironment").getOrElse(true)
    if (proxyRequired) Some(parseProxyConfiguration(configPrefix, configuration)) else None
  }

  def apply(configPrefix: String): Option[AkkaProxyServer] = {
    import play.api.Play.current
    apply(configPrefix, Play.configuration)
  }

  private def parseProxyConfiguration(configPrefix: String, configuration: Configuration) = {
    DefaultWSProxyServer(
      protocol = configuration.getString(s"$configPrefix.protocol").orElse(throw ProxyConfigurationException("protocol")),
      host = configuration.getString(s"$configPrefix.host").getOrElse(throw ProxyConfigurationException("host")),
      port = configuration.getInt(s"$configPrefix.port").getOrElse(throw ProxyConfigurationException("port")),
      principal = configuration.getString(s"$configPrefix.username"),
      password = configuration.getString(s"$configPrefix.password")
    )
  }

  case class ProxyConfigurationException(key: String) extends RuntimeException(s"Missing proxy configuration - key '$key' not found")

}

trait AkkaProxyServer {
  /** The hostname of the proxy server. */
  def host: String
  /** The port of the proxy server. */
  def port: Int
  /** The protocol of the proxy server.  Use "http" or "https".  Defaults to "http" if not specified. */
  def protocol: Option[String]
  /** The principal (aka username) of the credentials for the proxy server. */
  def principal: Option[String]
  /** The password for the credentials for the proxy server. */
  def password: Option[String]
  def ntlmDomain: Option[String]
  /** The realm's charset. */
  def encoding: Option[String]
  def nonProxyHosts: Option[Seq[String]]
}

case class DefaultWSProxyServer(
   /** The hostname of the proxy server. */
   host: String,
   /** The port of the proxy server. */
   port: Int,
   /** The protocol of the proxy server.  Use "http" or "https".  Defaults to "http" if not specified. */
   protocol: Option[String] = None,
   /** The principal (aka username) of the credentials for the proxy server. */
   principal: Option[String] = None,
   /** The password for the credentials for the proxy server. */
   password: Option[String] = None,
   ntlmDomain: Option[String] = None,
   /** The realm's charset. */
   encoding: Option[String] = None,
   nonProxyHosts: Option[Seq[String]] = None) extends AkkaProxyServer