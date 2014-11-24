package controllers

import java.util.concurrent.TimeUnit

import controllers.Auth.{baseUserForm, userForm}
import models.Person
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}
import utils.FamilySearch
import scala.concurrent.ExecutionContext.Implicits.global
import models.PersonWrites.fullReads
import scala.concurrent._
import scala.concurrent.duration.Duration

object DuplicateFinder extends Controller {

  def findDuplicateDetails = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid
    val nameList = userForm.bindFromRequest.get.nameList

    Ok(views.html.duplicatedetails(token, pid, nameList))
  }

  def findDuplicates = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid
    val nameList = userForm.bindFromRequest.get.nameList

    val allPeople = Json.parse(nameList).as[List[Person]]

    var ancestryNumberToPersonMap: Map[String, Person] = Map()

    allPeople.foreach(p => {
      ancestryNumberToPersonMap += p.ancestryNumber -> p
    })

    allPeople.foreach(p => {
      var num = -1
      if (!p.ancestryNumber.contains("JsUndefined") && p.ancestryNumber != "")
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
        if (parent.name == otherParent.name && parent.pid != otherParent.pid && !duplicatePids.contains(parent.pid) && !duplicatePids.contains(otherParent.pid)) {
          duplicatePids += otherParent.pid
          duplicates = (parent, otherParent) :: duplicates
        }
      })
    })

//    var detailedDuplicates: List[(Person, Person)] = List()
//    var duplicateFutures: List[Future[(Person,Person)]] = List()
//    duplicates.foreach(pair=>{
//      duplicateFutures = future { (FamilySearch.getPerson(token, pair._1.pid),
//        FamilySearch.getPerson(token, pair._2.pid)) } :: duplicateFutures
//    })
//
//    val f = Future.sequence(duplicateFutures).map(futureList => futureList.foreach(futureTuple=> {
//      detailedDuplicates = futureTuple :: detailedDuplicates
//    }))
//
//    Await.result(f,Duration(90,TimeUnit.SECONDS))

    duplicates = duplicates.sortBy(_._1.name)

    Ok(views.html.duplicates(duplicates))
  }

}
