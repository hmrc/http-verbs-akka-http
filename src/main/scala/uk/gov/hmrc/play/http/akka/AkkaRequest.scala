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
import akka.http.scaladsl.model.{HttpHeader, HttpMethod, HttpRequest, HttpResponse}
import akka.http.scaladsl.settings.ConnectionPoolSettings
import akka.http.scaladsl.{ClientTransport, Http, HttpExt}
import akka.stream.Materializer
import com.typesafe.config.Config
import play.api.libs.json.JsNull
import play.api.{Configuration, Play}
import uk.gov.hmrc.http.{HeaderCarrier, Request}

import scala.collection.immutable
import scala.concurrent.Future
import scala.concurrent.duration._

import play.api.libs.concurrent.Execution.Implicits.defaultContext

trait AkkaRequest extends Request {
  implicit def system: ActorSystem
  implicit def materializer: Materializer

  override lazy val configuration: Option[Config] = Play.maybeApplication.map(_.configuration.underlying)

  def akkaClient: HttpExt = Http()

  def doRequest(request: HttpRequest): Future[AkkaHttpResponse] =
    akkaClient.singleRequest(request).flatMap(toResponse)

  def buildRequest[A](url: String, method: HttpMethod)(implicit hc: HeaderCarrier): HttpRequest = {
    val headers = applicableHeaders(url)(hc)
      .map { case (key, value) => HttpHeader.parse(key, value) }
      .collect { case Ok(h, _) => h }
      .to[immutable.Seq]

    HttpRequest(uri = url, method = method).withHeaders(headers)
  }

  protected def toResponse(response: HttpResponse) : Future[AkkaHttpResponse] =
    response.entity.toStrict(300 millis).map(entity => AkkaHttpResponse(
      response.headers.map(h => (h.name(), Seq(h.value()))).toMap,
      response.status.intValue(),
      JsNull,
      entity.data.utf8String))

}

trait AkkaProxy extends AkkaRequest {
  def akkaProxyServer: Option[AkkaProxyServer]

  lazy val httpsProxyTransport: Option[ClientTransport] = akkaProxyServer map (
    proxy =>
      ClientTransport.httpsProxy(
        InetSocketAddress.createUnresolved(proxy.host, proxy.port),
        BasicHttpCredentials(proxy.principal.get, proxy.password.get))
    )

  lazy val settings: ConnectionPoolSettings = httpsProxyTransport
    .fold(ConnectionPoolSettings(system))(transport => ConnectionPoolSettings(system).withTransport(transport))

  override def doRequest(request: HttpRequest): Future[AkkaHttpResponse] = {
    akkaProxyServer match {
      case Some(_) =>
        import play.api.libs.concurrent.Execution.Implicits.defaultContext
        akkaClient.singleRequest(request, settings = settings).flatMap(toResponse)
      case None => super.doRequest(request)
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
    DefaultAkkaProxyServer(
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

case class DefaultAkkaProxyServer(
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