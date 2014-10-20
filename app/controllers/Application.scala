package controllers

import java.util.concurrent.TimeUnit

import models.Person
import play.api.Play
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent._

object Application extends Controller {

  val FAMILYSEARCH_DEVELOPER_ID: String = Play.current.configuration.getString("familysearch.developer.id").get
  val FAMILYSEARCH_SERVER_URL: String = Play.current.configuration.getString("familysearch.server.url").get

  def index = Action {
    Ok(views.html.index())
  }

  def privacyPolicy = Action {
    Ok(views.html.privacy())
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

    Await.result(future, Duration(5, java.util.concurrent.TimeUnit.SECONDS))
    val json = Json.parse(ret)
    var token: String = (json \ "access_token").toString
    token = "Bearer " + token.replaceAll("\"", "")

    val user = getCurrentUser(token)
    val j = Json.parse(user)
    val jsarray = j \ "users"
    val personId = (jsarray(0) \ "personId").toString().replace("\"", "")
    val displayName = (jsarray(0) \ "displayName").toString().replace("\"", "")
    Ok(views.html.menu(token, personId))
  }

  def getCurrentUser(token: String) = {
    var ret = ""
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/users/current")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      ret = response.body
    }

    Await.result(future, Duration(25, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getCousins() = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid

    var addedCousins: Map[String, Person] = Map[String, Person]()
    val grandparentSet = Person("Grandparent Set", "", "", None, false)

    //Walk up the tree
    val parents: List[Person] = getParents(token, Person("doesn't matter", pid, "", None))
    val grandparents = parents.foldLeft(List[Person]())((acc, item) => getParents(token, item) ::: acc)

    //Walk back down the tree

    //Get Aunts and Uncles
    var auntUncleFutures = List[Future[List[Person]]]()
    grandparents.foreach((item) => {
      auntUncleFutures = future { getChildren(token, pid, item) } :: auntUncleFutures
    })

    val f = Future.sequence(auntUncleFutures).map {
      lst => lst.foreach(l=>grandparentSet.addDescendants(l))
    }
    Await.result(f, Duration(5,TimeUnit.SECONDS))

    //auntsUncles = auntsUncles diff parents
    val cousins = grandparentSet.getDescendants().foldLeft(List[Person]())((acc, item) => {
      var children = getChildren(token, pid, item)
      var toRemove = List[Person]()
      children.foreach((child) => {
        val addedCousin = addedCousins.get(child.pid).getOrElse(Person("", "", "", None))
        if (addedCousin.getPid == "") {
          addedCousins += (child.getPid() -> child)
        }
        else {
          addedCousin.parent.get.altName = " & " + item.name
          toRemove = child :: toRemove
        }

      })
      toRemove.foreach((person) => children = children diff List(person))
      item.addDescendants(children)
      children ::: acc
    })

    var cousinList = cousins.foldLeft(List[Person]())((acc, item) => {
      if (item.pid != pid)
        item :: acc
      else
        acc
    })

    cousinList = cousinList.sortBy(_.getName())

    val gps = Person("All Grandparents", "", "", None)
    // Only save aunts/uncles that have descendants
    grandparentSet.getDescendants().filter(p=> {
      p.children.size > 0
    }).foreach(p=>{
      gps.addDescendant(p)
    })
    val json = gps.toJson

    Ok(views.html.cousins(cousinList, cousinList.size, json.toString()))
  }

  def getParents(token: String, person: Person): List[Person] = {
    var ret: List[Person] = List[Person]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/" + person.getPid() + "/parents")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      val body = response.body
      val json = Json.parse(body)
      val jsarray = json \ "persons"
      ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc, item) => {
        val id = (item \ "id").toString().replaceAll("\"", "")
        val name = (item \ "display" \ "name").toString().replaceAll("\"", "")
        val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
        Person(name, id, gender, None) :: acc
      })
    }
    Await.result(future, Duration(50, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getChildren(token: String, selfPid: String, parent: Person): List[Person] = {
    var ret: List[Person] = List[Person]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/" + parent.getPid + "/children")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      val body = response.body
      val json = Json.parse(body)
      val jsarray = json \ "persons"
      ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
        val id = (item \ "id").toString().replaceAll("\"", "")
        val name = (item \ "display" \ "name").toString().replaceAll("\"", "")
        val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
        val highlight = id.equals(selfPid)
        Person(name, id, gender, Some[Person](parent), highlight) :: acc
      })
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