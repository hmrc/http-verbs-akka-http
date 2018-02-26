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

import akka.http.scaladsl.model.HttpMethods
import play.api.libs.json.Writes
import uk.gov.hmrc.http._

import scala.concurrent.Future

trait AkkaPatch extends CorePatch with PatchHttpTransport with AkkaRequest {
  override def doPatch[A](url: String, body: A)(implicit rds: Writes[A], hc: HeaderCarrier): Future[HttpResponse] = {
    val jsonBody = rds.writes(body).toString()
    doRequest(buildRequest(url, HttpMethods.PATCH).withEntity(jsonBody))
  }
}
