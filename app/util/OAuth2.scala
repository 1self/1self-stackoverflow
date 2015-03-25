package util

import org.joda.time.{DateTime, DateTimeZone}
import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.json.{JsValue, JsArray, Json}
import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller, Results}
import play.api.{Application, Play}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class OAuth2(application: Application) {
  lazy val soClientId = application.configuration.getString("stackoverlow.client.id").get
  lazy val soClientSecret = application.configuration.getString("stackoverlow.client.secret").get
  lazy val callbackBaseUrl = application.configuration.getString("callback.base.url").get

  def getAuthorizationUrl(redirectUri: String, scope: String, state: String): String = {
    val baseUrl = application.configuration.getString("stackoverlow.redirect.url").get
    baseUrl.format(soClientId, redirectUri, scope, state)
  }

  def getToken(code: String): Future[String] = {
    val tokenResponse = WS.url("https://stackexchange.com/oauth/access_token")(application).
      withQueryString("client_id" -> soClientId,
        "client_secret" -> soClientSecret,
        "code" -> code,
        "redirect_uri" -> s"$callbackBaseUrl/_oauth-callback").
      withHeaders(HeaderNames.ACCEPT -> MimeTypes.JSON).
      post(Results.EmptyContent())

    tokenResponse.flatMap { response =>
      val params = response.body.split("=")
      params.size match {
        case 2 => Future.successful(params(1))
        case _ => Future.failed[String](new IllegalStateException("Sod off!"))
      }
    }
  }
}

object OAuth2 extends Controller {
  lazy val oauth2 = new OAuth2(Play.current)


  def callback(codeOpt: Option[String] = None, stateOpt: Option[String] = None) = Action.async { implicit request =>
    (for {
      code <- codeOpt
      state <- stateOpt
      oauthState <- request.session.get("oauth-state")
    } yield {
        if (state == oauthState) {
          oauth2.getToken(code).map { accessToken =>
            val username = request.session.get("oneselfUsername").getOrElse("error")
            val token = request.session.get("registrationToken").getOrElse("error")

            Redirect(util.routes.OAuth2.success()).withSession("oauth-token" -> accessToken,
              "oneselfUsername" -> username, "registrationToken" -> token)
          }.recover {
            case ex: IllegalStateException => Unauthorized(ex.getMessage)
          }
        }
        else {
          Future.successful(BadRequest("Invalid Stackoverflow login"))
        }
      }).getOrElse(Future.successful(BadRequest("No parameters supplied")))
  }

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
      .post(jsonObject)
  };

  def getReputationCount(authToken: String) = {
    implicit val app = Play.current
    val stackOverflowAppKey = Play.application.configuration.getString("stackoverflow.app.key").get
    WS.url("https://api.stackexchange.com/2.2/me")
      .withHeaders(("Accept-Encoding", "gzip, deflate"))
      .withQueryString(("access_token", authToken.toString()))
      .withQueryString(("site", "stackoverflow"))
      .withQueryString(("key", stackOverflowAppKey))
      .get()
  }

  def getAnswersCount(authToken: String) = {
    implicit val app = Play.current
    val stackOverflowAppKey = Play.application.configuration.getString("stackoverflow.app.key").get
    WS.url("https://api.stackexchange.com/2.2/me/answers")
      .withHeaders(("Accept-Encoding", "gzip, deflate"))
      .withQueryString(("access_token", authToken.toString()))
      .withQueryString(("site", "stackoverflow"))
      .withQueryString(("key", stackOverflowAppKey))
      .get()
  }

  def getQuestionsCount(authToken: String) = {
    implicit val app = Play.current
    val stackOverflowAppKey = Play.application.configuration.getString("stackoverflow.app.key").get
    WS.url("https://api.stackexchange.com/2.2/me/questions")
      .withHeaders(("Accept-Encoding", "gzip, deflate"))
      .withQueryString(("access_token", authToken.toString()))
      .withQueryString(("site", "stackoverflow"))
      .withQueryString(("key", stackOverflowAppKey))
      .get()
  }

  def convertTo1SelfEvents(reputationCount: String, answersCount: String, questionsCount: String): JsArray = {
    val dateTimeString = new DateTime(DateTimeZone.UTC).toString()

    val reputationEvent = Json.obj(
      "source" -> "1self-stackoverflow",
      "version" -> "0.0.1",
      "dateTime" -> dateTimeString,
      "objectTags" -> Json.arr("internet", "social-network", "stackoverflow", "reputation"),
      "actionTags" -> Json.arr("sample"),
      "properties" -> Json.obj("source" -> "1self-stackoverflow", "count" -> reputationCount.toString)
    )

    val answersEvent = Json.obj(
      "source" -> "1self-stackoverflow",
      "version" -> "0.0.1",
      "dateTime" -> dateTimeString,
      "objectTags" -> Json.arr("internet", "social-network", "stackoverflow", "answers"),
      "actionTags" -> Json.arr("sample"),
      "properties" -> Json.obj("source" -> "1self-stackoverflow", "count" -> answersCount.toString)
    )

    val questionsEvent = Json.obj(
      "source" -> "1self-stackoverflow",
      "version" -> "0.0.1",
      "dateTime" -> dateTimeString,
      "objectTags" -> Json.arr("internet", "social-network", "stackoverflow", "questions"),
      "actionTags" -> Json.arr("sample"),
      "properties" -> Json.obj("source" -> "1self-stackoverflow", "count" -> questionsCount.toString)
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
      .post(events.toString()).map { response =>
      println(response.json)
    }
  }

  def success() = Action.async { request =>
    implicit val app = Play.current
    lazy val callbackBaseUrl = Play.application.configuration.getString("callback.base.url").get
    lazy val apiBaseUrl = Play.application.configuration.getString("api.base.url").get

    request.session.get("oauth-token").fold(Future.successful(Unauthorized("No way! Error occurred"))) {

      authToken =>
        val oneselfUsername = request.session.get("oneselfUsername").getOrElse("invalid");
        val registrationToken = request.session.get("registrationToken").getOrElse("invalid");

        val callbackUrl = callbackBaseUrl + "/sync?username=" + oneselfUsername + "&auth_token=" + authToken + "&latestSyncField={{latestSyncField}}&streamid={{streamid}}"

        getReputationCount(authToken).map {
          response1 =>
            val reputation = (response1.json \ "items")(0) \ "reputation"
            val reputationCount = reputation.toString()
            println(reputationCount)
            println("Reputation count")
            println(reputationCount);

            getAnswersCount(authToken).map {
              response2 =>
                val answerItems = (response2.json \\ "items")

                val answers = answerItems map { q =>
                  q.as[List[JsValue]].size
                }
                val answersCount = answers(0).toString()

                println("Answers count  ", answersCount)

                getQuestionsCount(authToken).map {
                  response3 =>
                    val questionItems = (response3.json \\ "items")

                    val questions = questionItems map { q =>
                      q.as[List[JsValue]].size
                    }
                    val questionsCount = questions(0).toString()

                    val stream = registerStream(oneselfUsername, registrationToken, callbackUrl)

                    stream.map { response =>

                      val streamJson = response.json
                      val streamId = streamJson.\("streamid").toString()
                      val writeToken = streamJson.\("writeToken").toString()

                      println("Converting to 1self events format")
                      val events = convertTo1SelfEvents(reputationCount, answersCount, questionsCount)
                      println("Events is ", events)
                      sendToOneSelf(streamId, writeToken, events)
                    }
                }
            }
        }
        val redirectUrl = apiBaseUrl + "/integrations"
        Future {
          Redirect(redirectUrl, 302)
        }
    }
  }

  def sync() = Action.async { request =>
    implicit val app = Play.current

    val streamId = request.getQueryString("streamid").get
    //    val username = request.getQueryString("username").get
    val authToken = request.getQueryString("auth_token").get

    val writeToken = request.headers.get("Authorization").get

    getReputationCount(authToken).map {
      response1 =>
        val reputation = (response1.json \ "items")(0) \ "reputation"
        val reputationCount = reputation.toString()
        println(reputationCount)
        println("Reputation count")
        println(reputationCount);

        getAnswersCount(authToken).map {
          response2 =>
            val answerItems = (response2.json \\ "items")

            val answers = answerItems map { q =>
              q.as[List[JsValue]].size
            }
            val answersCount = answers(0).toString()

            println("Answers count  ", answersCount)

            getQuestionsCount(authToken).map {
              response3 =>
                val questionItems = (response3.json \\ "items")

                val questions = questionItems map { q =>
                  q.as[List[JsValue]].size
                }
                val questionsCount = questions(0).toString()

                println("Questions count  ", questionsCount)

                println("Converting to 1self events format")
                val events = convertTo1SelfEvents(reputationCount, answersCount, questionsCount)
                println("Events is ", events)
                sendToOneSelf(streamId, writeToken, events)
            }
        }
    }

    Future {
      Ok("done")
    }
  }
}
