package controllers

import java.util.concurrent.TimeUnit

import models.Person
import org.apache.commons.codec.digest.DigestUtils
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.{JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc._
import utils.{Event, FamilySearch, Mongo, Timer}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import models.PersonWrites.fullReads
object Maps extends Controller {

  def index = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid
    val nameList = userForm.bindFromRequest.get.nameList

    Event("Map")

    val generations = 5
    var allPeople = Json.parse(nameList).as[List[Person]]
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


    var json = "["
    var placeMap: Map[String, (String, String)] = Map()

    listOfPaths.foreach(path => {
      json += "["
      path.foreach(p => {
        //val detailedPerson = getDetailedPerson(p.pid, peopleDetails)

        if (p.place != null && p.place != "?" && !p.place.contains("JsUndefined")) {
          val sanitizedPlace = getEscapedString(p.place.trim.replaceAll("  ", " ").replaceAll(",,", ","))
          val hashedPlace = DigestUtils.md5(sanitizedPlace).toString
        //  val latlon = getLatLong(sanitizedPlace, hashedPlace, placeMap)
        //  placeMap = latlon._3
        //  if (latlon._1 != "" && latlon._2 != "") {
           // println(p.place + " vs " + place + " : " + latlon._1 + "," + latlon._2)
            json = json + "{name:\"" + p.name + "\",place:\"" + sanitizedPlace + "\", hash:\""+hashedPlace+"\", lat:\"" + "" + "\", lon:\"" + "" + "\", foundAddress:"+"false"+", link:\"" + p.link + "\"},"
         // }
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

  def getLatLong(address: String, hashedAddress: String, map: Map[String, (String, String)]): (String, String, Map[String, (String, String)], Boolean) = {
    val timer = Timer("getGeocode " + address)
    var lat = ""
    var lon = ""
    var newMap = map
    var foundAddress = false
    map.get(address) match {

      case Some(x) =>
        (x._1, x._2, map, foundAddress)
      case None =>
        val latlon = Mongo.getLatLon(hashedAddress)
        if (latlon == null) {
          lat = address
          lon = address
        } else {
          foundAddress = true
          lat = latlon._1
          lon = latlon._2
          newMap += address ->(lat, lon)
        }
        (lat, lon, newMap, foundAddress)
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

