package controllers

import play.api.Play
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc._
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global

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
    val map2 = Map("key" -> Seq("value"),"asdf"->Seq("asdf"))
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/cis-web/oauth2/v3/token")
    .post(requestMap).map { response =>
      ret = response.body

    }
    Await.result(future,Duration(5, java.util.concurrent.TimeUnit.SECONDS))
    val json = Json.parse(ret)
    var token: String = (json \ "access_token").toString

    token = token.replaceAll("\"","")
    val user = getCurrentUser("Bearer " + token)
    val j = Json.parse(user)
    val jsarray = j \ "users"
    val personId = jsarray(0) \ "personId"
    val displayName = jsarray(0) \ "displayName"
    val txt = "hello " + displayName + "(" + personId + ")"
    Ok(views.html.go(token))
  }

  def getCurrentUser(token: String) = {
    var ret = ""
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/users/current")
      .withHeaders(("Accept","application/x-fs-v1+json"),("Authorization",token))
      .get().map { response =>
        ret = response.body
    }
    Await.result(future, Duration(5, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getParents() = Action { implicit request =>
    val token: String = request.getQueryString("token").getOrElse("")
    val pid: String = request.getQueryString("pid").getOrElse("")
    val a:List[String] = reallyGetParents(token, pid)
    val grandparents = a.foldLeft(List[String]())((acc, item)=> reallyGetParents(token, item) ::: acc)
    var auntsUncles = grandparents.foldLeft(List[String]())((acc, item)=>reallyGetChildren(token, item, "id") ::: acc)
    auntsUncles = auntsUncles diff a
    val cousins = auntsUncles.foldLeft(List[String]())((acc, item)=>reallyGetChildren(token, item, "display") ::: acc)
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
  def reallyGetChildren(token: String, pid: String, field: String): List[String] = {
    var ret:List[String] = List[String]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/"+pid+"/children")
      .withHeaders(("Accept","application/x-fs-v1+json"),("Authorization",token))
      .get().map { response =>
      val body = response.body
      val json = Json.parse(body)
      val jsarray = json \ "persons"
      ret = jsarray.as[List[JsObject]].foldLeft(List[String]())((acc, item)=>(item \ field).toString().replaceAll("\"","") :: acc)
    }
    Await.result(future, Duration(50, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

}