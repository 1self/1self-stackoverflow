package controllers

import java.util.UUID

import play.api._
import play.api.mvc._
import util.OAuth2

object Application extends Controller {

  def index = Action { implicit request =>
    val oauth2 = new OAuth2(Play.current)

    val username = request.getQueryString("username").getOrElse("error")
    val token = request.getQueryString("token").getOrElse("error")

    val callbackUrl = util.routes.OAuth2.callback(None, None).absoluteURL()
    val scope = ""
    val state = UUID.randomUUID().toString // random confirmation string
    val redirectUrl = oauth2.getAuthorizationUrl(callbackUrl, scope, state)


    Ok(views.html.index("Your new application is ready.", redirectUrl)).
      withSession("oauth-state" -> state, "oneselfUsername" -> username, "registrationToken" -> token)
  }

}
