package controllers

import java.util.concurrent.TimeUnit

import models.Person
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc._
import utils.{FamilySearch, Mongo, Timer}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration

object Maps extends Controller {

  def index = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid
    val nameList = userForm.bindFromRequest.get.nameList

    val generations = 5
    var allPeople = FamilySearch.getAllPeople(generations, 0, pid, token).distinct
    val ascendencyMap = getAscendencyToPersonMap(allPeople)

    var listOfPaths: List[List[Person]] = List()

    (Math.pow(2, generations).toInt to Math.pow(2, generations + 1).toInt - 1).foreach(i => {
      val personGenerationPair = FamilySearch.getLastDescendent(ascendencyMap, i, generations)
      val person = personGenerationPair._1
      var generation = personGenerationPair._2
      var path: List[Person] = List(person)
      var geni = i
      while (geni > 1) {
        geni = geni / 2
        path = ascendencyMap.get(geni) match {
          case Some(x) => x :: path
          case None => path
        }
      }
      listOfPaths = path :: listOfPaths
    })


    allPeople = allPeople.distinct
    var peopleDetails: List[Person] = List()
    var duplicateFutures: List[Future[Person]] = List()
    allPeople.foreach(p => {
      duplicateFutures = future {
        FamilySearch.getPerson(token, p.pid)
      } :: duplicateFutures
    })

    val f = Future.sequence(duplicateFutures).map(futureList => futureList.foreach(p => {
      peopleDetails = p :: peopleDetails
    }))

    Await.result(f, Duration(900, TimeUnit.SECONDS))

    var json = "["
    var placeMap: Map[String, (String, String)] = Map()

    listOfPaths.foreach(path => {
      json += "["
      path.foreach(p => {
        val detailedPerson = getDetailedPerson(p.pid, peopleDetails)

        if (detailedPerson.place != null && detailedPerson.place != "?" && !detailedPerson.place.contains("JsUndefined")) {
          val place = detailedPerson.place.trim.replaceAll("  ", " ").replaceAll(",,", ",")
          val latlon = getLatLong(place, placeMap)
          placeMap = latlon._3
          if (latlon._1 != "" && latlon._2 != "") {
            println(detailedPerson.place + " vs " + place + " : " + latlon._1 + "," + latlon._2)
            json = json + "{name:\"" + detailedPerson.name + "\",place:\"" + place + "\", lat:\"" + latlon._1 + "\", lon:\"" + latlon._2 + "\",link:\"" + detailedPerson.link + "\"},"
          }
        }
      })
      json = json.substring(0, json.length - 1)
      json += "],"
    })
    json = json + "]"

    json = json.replace("'", "")

    Ok(views.html.map(json))
  }

  def hasOnlyAsciiLetters(str: String): Boolean = {
    val chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ,"
    var ret = true
    str.getBytes.foreach(b => {
      if (!chars.contains(b))
        ret = false
    })
    ret
  }

  def getEscapedString(str: String): String = {
    str.replaceAll("<", "").replaceAll(">", "").replaceAll(" ", "%20")
  }

  def getLatLong(address: String, map: Map[String, (String, String)]): (String, String, Map[String, (String, String)]) = {
    val timer = Timer("getGeocode " + address)
    var lat = ""
    var lon = ""
    var newMap = map
    map.get(address) match {

      case Some(x) =>
        (x._1, x._2, map)
      case None =>
        val latlon = Mongo.getLatLon(address)
        if (latlon == null) {
          Thread.sleep(150);
          //     ESCAPE THE ADDRESS MYSELF, INFER LOCATION?
          val future = WS.url("http://maps.googleapis.com/maps/api/geocode/json?sensor=false&address=" + getEscapedString(address))
            .get().map {
            response =>
              timer.logTime()
              val user = response.body
              val j = Json.parse(user)
              val jsarray = j \ "results"
              jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
                lat = (item \ "geometry" \ "location" \ "lat").toString().replaceAll("\"", "")
                lon = (item \ "geometry" \ "location" \ "lng").toString().replaceAll("\"", "")
                acc
              })
          }
          Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
          newMap += address ->(lat, lon)
          Mongo.addPlace(address, lat, lon)
        } else {
          lat = latlon._1
          lon = latlon._2
          newMap += address ->(lat, lon)
        }
        (lat, lon, newMap)
    }
  }

  def getDetailedPerson(pid: String, personList: List[Person]): Person = {
    var ret = Person("?")
    personList.foreach(p => {
      if (p.pid == pid) ret = p
    })
    ret
  }

  def getAscendencyToPersonMap(list: List[Person]): Map[Int, Person] = {
    var map: Map[Int, Person] = Map()
    list.foreach(p => {
      if (isInt(p.ancestryNumber))
        map += p.ancestryNumber.toInt -> p
    })
    map
  }

  def isInt(s: String): Boolean = {
    try {
      s.toInt
      true
    } catch {
      case e => false
    }
  }


  val userForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "nameList" -> text
    )(Cousins.apply)(Cousins.unapply)
  )
}

