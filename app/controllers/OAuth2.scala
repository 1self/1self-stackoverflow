package controllers

import play.api.http.{HeaderNames, MimeTypes}
import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller, Results}
import play.api.{Play}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class OAuth2(application: play.api.Application) {
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

            Redirect(routes.Application.success()).withSession("oauth-token" -> accessToken,
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

}
