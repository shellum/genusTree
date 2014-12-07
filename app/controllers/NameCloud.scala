package controllers

import controllers.Auth._
import models.{ColorScheme, Person, SimplePerson}
import org.apache.commons.lang3.StringEscapeUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import utils.{Event, FamilySearch}
import models.PersonWrites.fullReads

object NameCloud extends Controller {

  def nameCloud = Action { implicit request =>
    val token = nameCloudForm.bindFromRequest.get.token
    val pid = nameCloudForm.bindFromRequest.get.pid
    val generations = nameCloudForm.bindFromRequest.get.generations
    val unsanitizedFont = nameCloudForm.bindFromRequest.get.font
    val colorScheme = ColorScheme(nameCloudForm.bindFromRequest.get.colorScheme)
    val font = StringEscapeUtils.escapeHtml4(StringEscapeUtils.escapeEcmaScript(unsanitizedFont))
    val nameList = nameCloudForm.bindFromRequest.get.nameList

    Event("NameCloud")

    val allPeople = Json.parse(nameList).as[List[Person]]

    var partToNamesMap = Map[String, List[Person]]()

    var json = "["
    var nameMap = Map[String, Int]()
    allPeople.distinct.foreach(p => {
      val allButLastName = p.name.split(" ")
      (0 to allButLastName.length - 2).foreach(index => {
        val part = allButLastName(index)
        val upperCasePart = part.toUpperCase()
        val count = nameMap.get(upperCasePart)

        partToNamesMap.get(upperCasePart) match {
          case Some(x) => partToNamesMap += upperCasePart -> ((p) :: x)
          case _ => partToNamesMap += upperCasePart -> List(p)
        }

        if (part != "" && part.length > 2 && !excludedNames.contains(part.toLowerCase()))
          count match {
            case Some(x) => nameMap = nameMap + (part.toUpperCase() -> (nameMap.get(part.toUpperCase()).get + 5))
            case _ => nameMap = nameMap + (part.toUpperCase() -> 5)
          }
      })
    })

    var nameCount = 0
    var simplePersonList = List[SimplePerson]()

    nameMap.foreach(p => {
      simplePersonList = SimplePerson(p._1, p._2) :: simplePersonList
    })

    val sortedSimpleList = simplePersonList.sortWith(_.count > _.count)

    var maxSize = 0
    sortedSimpleList.foreach(p => {
      if (p.count > maxSize)
        maxSize = p.count
    })

    sortedSimpleList.foreach(p => {
      nameCount = nameCount + 1
      var nameSize = ((p.count * (65 - ((10 - generations) * 3))) / maxSize)
      //val smallestSize = 13 - (sortedSimpleList.size / 100)
      //if (nameSize < smallestSize) nameSize = smallestSize
      if (nameSize < 20 && nameSize > 10) nameSize -= 5
      else if (nameSize < 10) nameSize = 10
      if (nameCount < 150)
        json = json + "{name:\"" + p.name + "\",size:" + nameSize + "},"
    })

    if (nameCount < 100)
      do {
        sortedSimpleList.foreach(p => {
          nameCount = nameCount + 1
          json = json + "{name:\"" + p.name + "\",size:10},"
        })
      } while (nameCount < 150)

    var i = 30
    if (sortedSimpleList.size - 1 < 30) i = sortedSimpleList.size - 1
    (1 to i).foreach(i => {
      json = json + "{name:\"" + sortedSimpleList(i).name + "\",size:10},"
    })

    json = json.substring(0, json.length - 1)
    json = json + "]"

    val sortedPartToNamesMap = partToNamesMap.toList.sortBy(_._2.size).reverse
    Ok(views.html.namecloud(json, sortedPartToNamesMap, font, colorScheme, token))
  }

  def nameCloudDetails = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid
      val nameList = userForm.bindFromRequest.get.nameList

      Ok(views.html.nameclouddetails(token, pid, nameList))
    }
  }

  def excludedNames = List("stillborn", "stilborn", "still", "mr.", "mr", "miss", "miss.", "mrs", "mrs.")

  val nameCloudForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "generations" -> number,
      "font" -> text,
      "colorScheme" -> number,
      "nameList" -> text
    )(NameCloudParams.apply)(NameCloudParams.unapply)
  )

  case class NameCloudParams(token: String, pid: String, generations: Int, font: String, colorScheme: Int, nameList: String)

}
