package utils

import org.joda.time.{DateTime, DateTimeZone}
import play.api.Play
import play.api.libs.json.{JsNumber, JsArray, JsValue, Json}
import play.api.libs.ws.WS
import play.api.mvc.Controller
import scala.math.BigDecimal;

import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by anil on 25/03/15.
 */
object Oneself extends Controller {

  def registerStream(oneselfUsername: String, registrationToken: String, callbackUrl: String) = {
    println("In register stream")
    implicit val app = Play.current

    val apiBaseUrl = Play.application.configuration.getString("api.base.url").get
    val url = apiBaseUrl + "/v1/users/" + oneselfUsername + "/streams"

    val appId = Play.application.configuration.getString("app_id").get
    val appSecret = Play.application.configuration.getString("app_secret").get
    val authorizationKey = appId + ":" + appSecret

    val jsonObject = Json.toJson(Map("callbackUrl" -> callbackUrl))

    println("Created stream");
    WS.url(url)
      .withHeaders(("Authorization", authorizationKey))
      .withHeaders(("registration-token", registrationToken))
      .withHeaders(("Content-Type", "application/json"))
      .post(jsonObject).map { response =>

      val streamJson = response.json
      val streamId = streamJson.\("streamid").toString()
      val writeToken = streamJson.\("writeToken").toString()
      (streamId, writeToken)
    }
  };

  def getReputationCount(authToken: String) = {
    implicit val app = Play.current

    val stackOverflowAppKey = Play.application.configuration.getString("stackoverflow.app.key").get
    WS.url("https://api.stackexchange.com/2.2/me")
      .withHeaders(("Accept-Encoding", "gzip, deflate"))
      .withQueryString(("access_token", authToken.toString()))
      .withQueryString(("site", "stackoverflow"))
      .withQueryString(("key", stackOverflowAppKey))
      .get().map { res =>
      val reputation = (res.json \ "items")(0) \ "reputation"
      val reputationCount = BigDecimal(reputation.toString())
      reputationCount
    }
  }

  def getAnswersCount(authToken: String) = {
    implicit val app = Play.current
    val stackOverflowAppKey = Play.application.configuration.getString("stackoverflow.app.key").get
    WS.url("https://api.stackexchange.com/2.2/me/answers")
      .withHeaders(("Accept-Encoding", "gzip, deflate"))
      .withQueryString(("access_token", authToken.toString()))
      .withQueryString(("site", "stackoverflow"))
      .withQueryString(("key", stackOverflowAppKey))
      .get().map { res =>

      val answerItems = (res.json \\ "items")

      val answers = answerItems map { a =>
        a.as[List[JsValue]].size
      }
      val answersCount = BigDecimal(answers(0))

      answersCount
    }
  }

  def getQuestionsCount(authToken: String) = {
    implicit val app = Play.current
    val stackOverflowAppKey = Play.application.configuration.getString("stackoverflow.app.key").get
    WS.url("https://api.stackexchange.com/2.2/me/questions")
      .withHeaders(("Accept-Encoding", "gzip, deflate"))
      .withQueryString(("access_token", authToken.toString()))
      .withQueryString(("site", "stackoverflow"))
      .withQueryString(("key", stackOverflowAppKey))
      .get().map { res =>

      val questionItems = (res.json \\ "items")

      val questions = questionItems map { q =>
        q.as[List[JsValue]].size
      }
      val questionsCount = BigDecimal(questions(0))

      questionsCount
    }
  }

  def convertTo1SelfEvents(reputationCount: BigDecimal, questionsCount: BigDecimal, answersCount: BigDecimal) = {
    val dateTimeString = new DateTime(DateTimeZone.UTC).toString()

    val reputationEvent = Json.obj(
      "source" -> "1self-stackoverflow",
      "version" -> "0.0.1",
      "dateTime" -> dateTimeString,
      "objectTags" -> Json.arr("internet", "social-network", "stackoverflow", "reputation"),
      "actionTags" -> Json.arr("sample"),
      "properties" -> Json.obj("points" -> JsNumber(reputationCount))
    )

    val answersEvent = Json.obj(
      "source" -> "1self-stackoverflow",
      "version" -> "0.0.1",
      "dateTime" -> dateTimeString,
      "objectTags" -> Json.arr("internet", "social-network", "stackoverflow", "questions"),
      "actionTags" -> Json.arr("sample"),
      "properties" -> Json.obj("answered" -> JsNumber(answersCount))
    )

    val questionsEvent = Json.obj(
      "source" -> "1self-stackoverflow",
      "version" -> "0.0.1",
      "dateTime" -> dateTimeString,
      "objectTags" -> Json.arr("internet", "social-network", "stackoverflow", "questions"),
      "actionTags" -> Json.arr("sample"),
      "properties" -> Json.obj("asked" -> JsNumber(questionsCount))
    )

    val syncStartEvent = create_sync_start_event
    val syncCompleteEvent = create_sync_complete_event
    val events = Json.arr(syncStartEvent, reputationEvent, answersEvent, questionsEvent, syncCompleteEvent)
    events
  }

  def create_sync_start_event = {
    val event = Json.obj(
      "dateTime" -> Json.toJson(new DateTime(DateTimeZone.UTC).toString()),
      "objectTags" -> Json.arr("sync"),
      "actionTags" -> Json.arr("start"),
      "properties" -> Json.obj("source" -> "1self-stackoverflow")
    )
    event
  };

  def create_sync_complete_event = {
    val event = Json.obj(
      "dateTime" -> Json.toJson(new DateTime(DateTimeZone.UTC).toString()),
      "objectTags" -> Json.arr("sync"),
      "actionTags" -> Json.arr("complete"),
      "properties" -> Json.obj("source" -> "1self-stackoverflow")
    )
    event
  };

  def sendToOneSelf(streamId: String, writeToken: String, events: JsArray) = {
    implicit val app = Play.current
    println("Stream ID -> " + streamId)
    println("writeToken -> " + writeToken)
    val apiBaseUrl = Play.application.configuration.getString("api.base.url").get
    val url = apiBaseUrl + "/v1/streams/" + streamId + "/events/batch"
    val urlCorrected = url.replace("\"", "")
    val convertedWriteToken = writeToken.replace("\"", "")
    println("Sent to 1self successfully", urlCorrected)

    WS.url(urlCorrected)
      .withHeaders(("Authorization", convertedWriteToken))
      .withHeaders(("Content-Type", "application/json"))
      .post(events).map { response =>
      println(response.json)
    }
  }

}
