package controllers

import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.mvc._
import utils.FamilySearch

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global

object Auth extends Controller {

  def index = Action {
    Ok(views.html.index(FamilySearch.FAMILYSEARCH_IDENT_URL))
  }

  def privacyPolicy = Action {
    Ok(views.html.privacy())
  }

  def getNameList() = Action { implicit request =>
    val token = nameListForm.bindFromRequest.get.token
    val pid = nameListForm.bindFromRequest.get.pid
    val generations = nameListForm.bindFromRequest.get.generations

    val allPeople = FamilySearch.getAllPeople(5, pid, token).distinct
    var json = "["
    allPeople.distinct.foreach(p => {
      json = json + "{name:\"" + p.name + "\",pid:\"" + p.pid + "\"},"
    })
    json = json.substring(0, json.length - 1)
    json = json + "]"
    json = json.replace("'", "")
    Ok(json).as(TEXT)
  }

  def menu = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid
      val nameList = userForm.bindFromRequest.get.nameList

      Ok(views.html.menu(token, pid, nameList))
    }
  }

  def processToken = Action { implicit request =>
    var ret = "";
    val code: String = request.getQueryString("code").getOrElse("")
    val requestMap = Map("grant_type" -> Seq("authorization_code"),
      "code" -> Seq(code),
      "client_id" -> Seq(FamilySearch.FAMILYSEARCH_DEVELOPER_ID))
    val future = WS.url(FamilySearch.FAMILYSEARCH_IDENT_URL + "/cis-web/oauth2/v3/token")
      .post(requestMap).map { response =>
      ret = response.body
    }

    Await.result(future, Duration(30, java.util.concurrent.TimeUnit.SECONDS))
    val json = Json.parse(ret)

    (json \ "access_token").asOpt[String] match {
      case None => Redirect(routes.Auth.index())
      case Some(x) =>
        var token: String = (json \ "access_token").toString
        token = "Bearer " + token.replaceAll("\"", "")

        val user = getCurrentUser(token)
        val j = Json.parse(user)
        val jsarray = j \ "users"
        val personId = (jsarray(0) \ "personId").toString().replace("\"", "")
        val displayName = (jsarray(0) \ "displayName").toString().replace("\"", "")
        Ok(views.html.loading(token, personId))
    }
  }

  def getCurrentUser(token: String) = {
    var ret = ""
    val future = WS.url(FamilySearch.FAMILYSEARCH_SERVER_URL + "/platform/users/current")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      ret = response.body
    }

    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  val userForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "nameList" -> text
    )(Cousins.apply)(Cousins.unapply)
  )
  val baseUserForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text
    )(BaseForm.apply)(BaseForm.unapply)
  )
  val nameListForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "generations" -> number
    )(NameListForm.apply)(NameListForm.unapply)
  )

}

case class BaseForm(token: String, pid: String)

case class Cousins(token: String, pid: String, nameList: String)

case class NameListForm(token: String, pid: String, generations: Int)
