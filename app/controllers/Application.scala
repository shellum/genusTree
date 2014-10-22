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
import scala.concurrent._
import scala.concurrent.duration.Duration

object Application extends Controller {

  val FAMILYSEARCH_DEVELOPER_ID: String = Play.current.configuration.getString("familysearch.developer.id").get
  val FAMILYSEARCH_SERVER_URL: String = Play.current.configuration.getString("familysearch.server.url").get

  def index = Action {
    Ok(views.html.index(FAMILYSEARCH_SERVER_URL))
  }

  def privacyPolicy = Action {
    Ok(views.html.privacy())
  }

  def search = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid

      Ok(views.html.search(token, pid))
    }
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
      auntUncleFutures = future {
        getChildren(token, pid, item)
      } :: auntUncleFutures
    })

    val f = Future.sequence(auntUncleFutures).map {
      lst => lst.foreach(l => grandparentSet.addDescendants(l))
    }
    Await.result(f, Duration(50, TimeUnit.SECONDS))

    // Get cousins
    var cousinFutures = List[Future[List[Person]]]()
    grandparentSet.getDescendants().foreach((item) => {
      cousinFutures = future {
        getChildren(token, pid, item)
      } :: cousinFutures
    })

    var allCousins = List[Person]()
    val fz = Future.sequence(cousinFutures).map {
      auntUncleChildren: List[List[Person]] => {
        auntUncleChildren.foreach((singleAuntUncleChildren: List[Person]) => {
          var cousinsToAdd = singleAuntUncleChildren
          var toRemove = List[Person]()
          singleAuntUncleChildren.foreach((cousin) => {
            val addedCousin = addedCousins.get(cousin.pid).getOrElse(Person("", "", "", None))
            if (addedCousin.getPid == "") {
              addedCousins += (cousin.getPid() -> cousin)
            }
            else {
              addedCousin.parent.get.altName = " & " + cousin.parent.get.name
              toRemove = cousin :: toRemove
            }

          })
          toRemove.foreach((person) => cousinsToAdd = cousinsToAdd diff List(person))
          if (cousinsToAdd.size > 0) {
            getAuntUncleMatch(cousinsToAdd.head, grandparentSet.getDescendants()).addDescendants(cousinsToAdd)
          }
          allCousins = allCousins ::: cousinsToAdd
        })
      }
    }

    Await.result(fz, Duration(50, TimeUnit.SECONDS))

    var cousinList = allCousins.foldLeft(List[Person]())((acc, item) => {
      if (item.pid != pid)
        item :: acc
      else
        acc
    })

    cousinList = cousinList.sortBy(_.getName())

    val gps = Person("All Grandparents", "", "", None)
    // Only save aunts/uncles that have descendants
    grandparentSet.getDescendants().filter(p => {
      p.children.size > 0
    }).foreach(p => {
      gps.addDescendant(p)
    })
    val json = gps.toJson

    Ok(views.html.cousins(cousinList.sorted, cousinList.size, json.toString()))
  }

  def getAuntUncleMatch(cousin: Person, auntUncleList: List[Person]): Person = {
    var matchedAuntUncle: Person = null
    auntUncleList.foreach(auntUncle => {
      if (auntUncle.pid == cousin.parent.get.pid)
        matchedAuntUncle = auntUncle
    })
    matchedAuntUncle
  }

  def getParents(token: String, person: Person): List[Person] = {
    var ret: List[Person] = List[Person]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/" + person.getPid() + "/parents")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      val body = response.body
      body match {
        case "" =>
        case _ =>
          val json = Json.parse(body)
          val jsarray = json \ "persons"
          ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc, item) => {
            val id = (item \ "id").toString().replaceAll("\"", "")
            val name = (item \ "display" \ "name").toString().replaceAll("\"", "")
            val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
            Person(name, id, gender, None) :: acc
          })
      }
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