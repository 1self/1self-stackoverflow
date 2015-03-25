package controllers

import java.util.UUID

import play.api._
import play.api.libs.Crypto
import play.api.mvc._
import play.utils.UriEncoding
import utils.Oneself
import Oneself._

import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {

  def index = Action { implicit request =>
    val oauth2 = new OAuth2(Play.current)

    val username = request.getQueryString("username").getOrElse("error")
    val token = request.getQueryString("token").getOrElse("error")

    val callbackUrl = routes.OAuth2.callback(None, None).absoluteURL()
    val scope = "no_expiry"
    val state = UUID.randomUUID().toString // random confirmation string
  val redirectUrl = oauth2.getAuthorizationUrl(callbackUrl, scope, state)

    Redirect(redirectUrl).
      withSession("oauth-state" -> state, "oneselfUsername" -> username, "registrationToken" -> token)
  }

  def success = Action.async { request =>
    implicit val app = Play.current
    lazy val callbackBaseUrl = Play.application.configuration.getString("callback.base.url").get
    lazy val apiBaseUrl = Play.application.configuration.getString("api.base.url").get
    lazy val encryptionKey = Play.application.configuration.getString("encryption.key").get

    request.session.get("oauth-token").fold(Future.successful(Unauthorized("No way! Error occurred"))) {

      authToken =>
        val oneselfUsername = request.session.get("oneselfUsername").getOrElse("invalid");
        val registrationToken = request.session.get("registrationToken").getOrElse("invalid");

        val encryptedAuthToken = Crypto.encryptAES(authToken, encryptionKey)

        val encodedEncryptedAuthToken = UriEncoding.encodePathSegment(encryptedAuthToken, "UTF-8")

        val callbackUrl = callbackBaseUrl + "/sync?username=" + oneselfUsername + "&auth_token=" + encodedEncryptedAuthToken + "&latestSyncField={{latestSyncField}}&streamid={{streamid}}"

        for {
          reputationCount <- getReputationCount(authToken)
          answersCount <- getQuestionsCount(authToken)
          questionsCount <- getAnswersCount(authToken)
          streamResp <- registerStream(oneselfUsername, registrationToken, callbackUrl)
          events <- Future {
            convertTo1SelfEvents(reputationCount, answersCount, questionsCount)
          }
          _ <- sendToOneSelf(streamResp._1, streamResp._2, events)

        } yield Redirect(apiBaseUrl + "/integrations", 302)
    }
  }

  def sync = Action.async { request =>
    implicit val app = Play.current
    lazy val encryptionKey = Play.application.configuration.getString("encryption.key").get

    val streamId = request.getQueryString("streamid").get
    //    val username = request.getQueryString("username").get
    val encryptedAuthToken = request.getQueryString("auth_token").get
    val writeToken = request.headers.get("Authorization").get

    val authToken = Crypto.decryptAES(encryptedAuthToken, encryptionKey)

    for {
      reputationCount <- getReputationCount(authToken)
      answersCount <- getQuestionsCount(authToken)
      questionsCount <- getAnswersCount(authToken)
      events = convertTo1SelfEvents(reputationCount, answersCount, questionsCount)
      _ <- sendToOneSelf(streamId, writeToken, events)

    } yield Ok("Done")

  }

}
