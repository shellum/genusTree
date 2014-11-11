package utils

import java.util.concurrent.TimeUnit

import models.Person
import play.api.Play
import play.api.libs.json.{JsObject, JsUndefined, Json}
import play.api.libs.ws.WS

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration


object FamilySearch {

  val FAMILYSEARCH_DEVELOPER_ID: String = Play.current.configuration.getString("familysearch.developer.id").get
  val FAMILYSEARCH_SERVER_URL: String = Play.current.configuration.getString("familysearch.server.url").get
  val FAMILYSEARCH_IDENT_URL: String = Play.current.configuration.getString("familysearch.ident.url").get

  val API_URL_DESCENDANCY: String = "/platform/tree/descendancy"
  val API_URL_ANCESTRY: String = "/platform/tree/ancestry"
  val API_URL_PERSONS: String = "/platform/tree/persons/"


  def getChildren(token: String, selfPid: String, parent: Person): List[Person] = {
    var ret: List[Person] = List[Person]()
    val timer = Timer("getChildren")
    val future = WS.url(FAMILYSEARCH_SERVER_URL + API_URL_PERSONS + parent.getPid + "/children")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      timer.logTime()
      val body = response.body
      body match {
        case "" =>
        case "{\n  \"errors\" : [ {\n    \"code\" : 429\n  } ]\n}" => Event("throttled")
        case _ =>
          val json = Json.parse(body)
          val jsarray = json \ "persons"
          ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
            val id = (item \ "id").toString().replaceAll("\"", "")
            val name = (item \ "display" \ "name").toString().replaceAll("\"", "")
            val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
            val link = (item \ "identifiers" \ "http://gedcomx.org/Persistent")(0).toString().replaceAll("\"", "")
            val firstName = ((item \ "names")(0))
            val firsts = (firstName \ "nameForms")(0)
            val fir = (firsts \ "parts")
            val firstNamez = (fir(0) \ "value").toString().replaceAll("\"", "").split(" ")(0)
            val highlight = id.equals(selfPid)
            val person = Person(id, name, gender, Some[Person](parent), highlight = highlight, link = link, firstName = firstNamez)
            person :: acc
          })
      }
    }
    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getAncestors(token: String, selfPid: String, generations: Int, url: String): List[Person] = {
    var ret: List[Person] = List[Person]()
    var what = "getAncestors"
    if (url.contains("descendancy"))
      what = "getDescendants"
    val timer = Timer(what)
    val future = WS.url(FAMILYSEARCH_SERVER_URL + url + "?person=" + selfPid + "&generations=" + generations)
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      timer.logTime()
      val body = response.body
      body match {
        case "" =>
        case "{\n  \"errors\" : [ {\n    \"code\" : 429\n  } ]\n}" => Event("throttled")
        case _ =>
          val json = Json.parse(body)
          val jsarray = json \ "persons"
          ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
            val id = (item \ "id").toString().replaceAll("\"", "")
            val name = (item \ "display" \ "name")
            val nameText = name match {
              case x: JsUndefined => "Unknown";
              case x => x.toString().replaceAll("\"", "").replace("\\", "");
            }
            val ancesteryNumber = item \ "display" \ "ascendancyNumber"
            var ancesteryNumberStr = ""
            if (ancesteryNumber != JsUndefined) ancesteryNumberStr = ancesteryNumber.toString().replaceAll("\"", "")
            val descendancyNumber = item \ "display" \ "descendancyNumber"
            var descendancyNumberStr = ""
            if (descendancyNumber != JsUndefined) descendancyNumberStr = descendancyNumber.toString().replaceAll("\"", "")
            val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
            val link = "https://familysearch.org/ark:/61903/4:1:" + id
            val firstName = (item \ "names")(0)
            val firsts = (firstName \ "nameForms")(0)
            val fir = firsts \ "parts"
            val firstNamez = (fir(0) \ "value").toString().replaceAll("\"", "").replace("\\", "").split(" ")(0)
            val person = Person(id, nameText, gender, None, link = link, firstName = firstNamez, ancestryNumber = ancesteryNumberStr, descendancyNumber = descendancyNumberStr)
            person :: acc
          })
      }
    }
    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getDescendants(token: String, pid: String, generations: Int): List[Person] = {
    var allPeople: List[Person] = List()
    var nextGenToFollow: List[Person] = List(Person(pid))

    // Off case fix
    var fixedGenerations = generations
    if (generations == 2) fixedGenerations = 4

    (1 to (fixedGenerations + 1) / 2).reverse.foreach(i => {
      val generationsToGet = i match {
        case 1 => 1
        case _ => 2
      }
      var descendantFutures: List[Future[List[Person]]] = List()
      nextGenToFollow.foreach(p => {
        descendantFutures = future {
          getAncestors(token, p.pid, generationsToGet, API_URL_DESCENDANCY)
        } :: descendantFutures
      })

      var generationList: List[Person] = List()
      val f = Future.sequence(descendantFutures).map(futureList => futureList.foreach(singleFuture => {
        allPeople = singleFuture ::: allPeople
        generationList = singleFuture ::: generationList
      }))
      Await.result(f, Duration(90, TimeUnit.SECONDS))

      nextGenToFollow = generationList
    })

    allPeople
  }

  def getAllPeople(ascendingGenerations: Int, descendingGenerations: Int, pid: String, token: String): List[Person] = {
    var allPeople: List[Person] = List[Person]()
    val ancestors: List[Person] = getAncestors(token, pid, ascendingGenerations, API_URL_ANCESTRY)

    allPeople = ancestors.distinct

    var ancestryNumberToPersonMap: Map[String, Person] = Map()
    ancestors.foreach(p => {
      ancestryNumberToPersonMap += p.ancestryNumber -> p
    })

    var descendantFutures: List[Future[List[Person]]] = List()
    allPeople.foreach(p => {
      descendantFutures = future {
        getDescendants(token, p.pid, descendingGenerations)
      } :: descendantFutures
    })

    val f = Future.sequence(descendantFutures).map(futureList => futureList.foreach(singleFuture =>
      allPeople = (singleFuture ::: allPeople).distinct)
    )
    Await.result(f, Duration(90, TimeUnit.SECONDS))

    allPeople.distinct
  }

  def getLastDescendent(map: Map[String, Person], num: Int, generation: Int): (Person, Int) = {
    val a = map.get(num.toString)
    a match {
      case None =>
        getLastDescendent(map, num / 2, generation - 1)
      case Some(p) =>
        (p, generation)
    }
  }

  def getCurrentUser(token: String) = {
    var ret = ""
    val timer = Timer("getCurrentUser")
    val future = WS.url(FamilySearch.FAMILYSEARCH_SERVER_URL + "/platform/users/current")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      timer.logTime()
      ret = response.body
    }

    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getPerson(token: String, personId: String) = {
    var ret = Person("")
    val timer = Timer("getPerson")
    val future = WS.url(FamilySearch.FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/" + personId)
      .withHeaders(("Accept", "application/x-gedcomx-v1+json"), ("Authorization", token))
      .get().map { response =>
      timer.logTime()
      val user = response.body
      val j = Json.parse(user)
      val jsarray = j \ "persons"
      jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
        val id = (item \ "id").toString().replaceAll("\"", "")
        val name = (item \ "display" \ "name")
        val nameText = name match {
          case x: JsUndefined => "Unknown";
          case x => x.toString().replaceAll("\"", "").replace("\\", "");
        }
        val lifespan = (item \ "display" \ "lifespan")
        val lifespanText = lifespan match {
          case x: JsUndefined => "Unknown";
          case x => x.toString().replaceAll("\"", "").replace("\\", "");
        }
        val link = "https://familysearch.org/ark:/61903/4:1:" + id
        ret = Person(personId, nameText,link=link)
        ret.setLifespan(lifespanText)
        acc
      })
    }

    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

}
