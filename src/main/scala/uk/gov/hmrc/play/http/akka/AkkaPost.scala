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

import play.api.libs.json.{Json, Writes}
import play.api.mvc.Results
import uk.gov.hmrc.http._

import scala.concurrent.Future


trait AkkaPost extends CorePost with PostHttpTransport with AkkaRequest {

  override def doPost[A](url: String, body: A, headers: Seq[(String,String)])(implicit rds: Writes[A], hc: HeaderCarrier): Future[HttpResponse] = {

    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    buildRequest(url).withHeaders(headers: _*).post(Json.toJson(body)).map(new AkkaHttpResponse(_))
  }

  override def doFormPost(url: String, body: Map[String,Seq[String]])(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    buildRequest(url).post(body).map(new AkkaHttpResponse(_))
  }

  override def doPostString(url: String, body: String, headers: Seq[(String,String)])(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    buildRequest(url).withHeaders(headers: _*).post(body).map(new AkkaHttpResponse(_))
  }

  override def doEmptyPost[A](url: String)(implicit hc: HeaderCarrier): Future[HttpResponse] = {
    import play.api.http.Writeable._
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    buildRequest(url).post(Results.EmptyContent()).map(new AkkaHttpResponse(_))
  }
}
