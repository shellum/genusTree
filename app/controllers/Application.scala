package controllers

import play.api.Play
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global
import play.api.data.Forms._
import play.api.data._
import play.api.mvc._

object Application extends Controller {

  val FAMILYSEARCH_DEVELOPER_ID: String = Play.current.configuration.getString("familysearch.developer.id").get
  val FAMILYSEARCH_SERVER_URL: String = Play.current.configuration.getString("familysearch.server.url").get

  def index = Action {
    Ok(views.html.index())
  }

  def processToken = Action { implicit request =>

    var ret = "";
    val code: String = request.getQueryString("code").getOrElse("")
    val requestMap = Map("grant_type" -> Seq("authorization_code"),
      "code" -> Seq(code),
      "client_id" -> Seq(FAMILYSEARCH_DEVELOPER_ID))
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/cis-web/oauth2/v3/token")
    .post(requestMap).map { response =>
      ret = response.body
    }

    Await.result(future,Duration(5, java.util.concurrent.TimeUnit.SECONDS))
    val json = Json.parse(ret)
    var token: String = (json \ "access_token").toString
    token = "Bearer " + token.replaceAll("\"","")

    val user = getCurrentUser(token)
    val j = Json.parse(user)
    val jsarray = j \ "users"
    val personId = (jsarray(0) \ "personId").toString().replace("\"","")
    val displayName = (jsarray(0) \ "displayName").toString().replace("\"","")
    Ok(views.html.menu(token, personId))
  }

  def getCurrentUser(token: String) = {
    var ret = ""
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/users/current")
      .withHeaders(("Accept","application/x-fs-v1+json"),("Authorization",token))
      .get().map { response =>
        ret = response.body
    }

    Await.result(future, Duration(25, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getParents() = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid
    val parents:List[String] = reallyGetParents(token, pid)
    val grandparents = parents.foldLeft(List[String]())((acc, item)=> reallyGetParents(token, item) ::: acc)
    val funId = (acc: List[String], item: JsObject)=>(item \ "id").toString().replaceAll("\"","") :: acc
    val funDisplay = (acc: List[String], item: JsObject)=>(item \ "display" \ "name").toString().replaceAll("\"","") :: acc
    var auntsUncles = grandparents.foldLeft(List[String]())((acc, item)=>reallyGetChildren(token, item, funId) ::: acc)
    auntsUncles = auntsUncles diff parents
    val cousins = auntsUncles.foldLeft(List[String]())((acc, item)=>reallyGetChildren(token, item, funDisplay) ::: acc)
    val cousinset = cousins.foldLeft(Set[String]())((acc, item)=>acc + item)

    Ok(views.html.cousins(cousinset))
  }

  def reallyGetParents(token: String, pid: String): List[String] = {
    var ret:List[String] = List[String]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/"+pid+"/parents")
      .withHeaders(("Accept","application/x-fs-v1+json"),("Authorization",token))
      .get().map { response =>
      val body = response.body
      val json = Json.parse(body)
      val jsarray = json \ "persons"
      ret = jsarray.as[List[JsObject]].foldLeft(List[String]())((acc, item)=>(item \ "id").toString().replaceAll("\"","") :: acc)
    }
    Await.result(future, Duration(50, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }
  def reallyGetChildren(token: String, pid: String, fun: (List[String],JsObject)=>List[String]): List[String] = {
    var ret:List[String] = List[String]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/"+pid+"/children")
      .withHeaders(("Accept","application/x-fs-v1+json"),("Authorization",token))
      .get().map { response =>
      val body = response.body
      val json = Json.parse(body)
      val jsarray = json \ "persons"
      ret = jsarray.as[List[JsObject]].foldLeft(List[String]())(fun)
    }
    Await.result(future, Duration(50, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  val userForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text
    )(Cousins.apply)(Cousins.unapply)
  )

}

case class Cousins(token: String, pid: String)