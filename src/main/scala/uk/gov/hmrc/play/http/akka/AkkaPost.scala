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

import akka.http.scaladsl.model.HttpHeader.ParsingResult.Ok
import akka.http.scaladsl.model.{FormData, HttpCharsets, HttpHeader, HttpMethods}
import play.api.libs.json.Writes
import uk.gov.hmrc.http._

import scala.collection.immutable
import scala.concurrent.Future

trait AkkaPost extends CorePost with PostHttpTransport with AkkaRequest {
  override def doPost[A](url: String, body: A, headers: Seq[(String, String)])(implicit rds: Writes[A], hc: HeaderCarrier): Future[HttpResponse] = {
    val jsonBody = rds.writes(body).toString()

    val hs = headers.map { case (key, value) => HttpHeader.parse(key, value) }
      .collect { case Ok(h, _) => h }
      .to[immutable.Seq]

    doRequest(buildRequest(url, HttpMethods.POST).withHeadersAndEntity(hs, jsonBody))
  }

  override def doFormPost(url: String, body: Map[String, Seq[String]])(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    val data = body.flatMap { case (key, value) => value.map(v => (key, v)) }

    doRequest(buildRequest(url, HttpMethods.POST)
      .withEntity(FormData(data).toEntity(HttpCharsets.`UTF-8`)))
  }

  override def doPostString(url: String, body: String, headers: Seq[(String, String)])(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    val hs = headers.map { case (key, value) => HttpHeader.parse(key, value) }
      .collect { case Ok(h, _) => h }
      .to[immutable.Seq]

    doRequest(buildRequest(url, HttpMethods.POST).withHeadersAndEntity(hs, body))
  }

  override def doEmptyPost[A](url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    doRequest(buildRequest(url, HttpMethods.POST))
  }
}
