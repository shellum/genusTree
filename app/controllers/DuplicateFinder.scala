package controllers

import models.Person
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}
import utils.FamilySearch

object DuplicateFinder extends Controller {

  def findDuplicateDetails = Action { implicit request =>
    val token = duplicateForm.bindFromRequest.get.token
    val pid = duplicateForm.bindFromRequest.get.pid

    Ok(views.html.duplicatedetails(token, pid))
  }

  def findDuplicates = Action { implicit request =>
    val token = duplicateForm.bindFromRequest.get.token
    val pid = duplicateForm.bindFromRequest.get.pid

    val allPeople = FamilySearch.getAllPeople(3, 2, pid, token).distinct

    var ancestryNumberToPersonMap: Map[String, Person] = Map()

    allPeople.foreach(p => {
      ancestryNumberToPersonMap += p.ancestryNumber -> p
    })

    allPeople.foreach(p => {
      var num = -1
      if (!p.ancestryNumber.contains("JsUndefined"))
        num = p.ancestryNumber.toInt
      ancestryNumberToPersonMap.get((num / 2).toString) match {
        case None => ;
        case Some(x) => p.addDescendant(x)
      }
    })

    var duplicates: List[(Person, Person)] = List()
    var duplicatePids: Set[String] = Set()

    allPeople.foreach(parent => {
      allPeople.foreach(otherParent => {
        if (parent.name.contains("Marie") && otherParent.name.contains("Marie") && parent.name.contains("B") && otherParent.name.contains("B")) {
          var j = parent.pid + "d"
          j = j + "dsf"
        }
        if (parent.name == otherParent.name && parent.pid != otherParent.pid && !duplicatePids.contains(parent.pid) && !duplicatePids.contains(otherParent.pid)) {
          duplicatePids += otherParent.pid
          duplicates = (parent, otherParent) :: duplicates
        }
      })
    })
    duplicates = duplicates.sortBy(_._1.name)

    Ok(views.html.duplicates(duplicates))
  }

  val duplicateForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text
    )(DuplicateParams.apply)(DuplicateParams.unapply)
  )

  case class DuplicateParams(token: String, pid: String)

}
