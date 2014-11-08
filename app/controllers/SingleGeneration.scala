package controllers

import java.util.concurrent.TimeUnit

import controllers.Auth._
import models.Person
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}
import utils.FamilySearch

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object SingleGeneration extends Controller {
  def search = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid
      val nameList = userForm.bindFromRequest.get.nameList

      Ok(views.html.search(token, pid, nameList))
    }
  }

  def getCousinTree(token: String, pid: String): (Person, List[Person]) = {
    var addedCousins: Map[String, Person] = Map[String, Person]()
    val grandparentSet = Person("")

    val grandparents = FamilySearch.getAncestors(token, pid, 2, FamilySearch.API_URL_ANCESTRY).filter(p => {
      !p.ancestryNumber.contains("S") && p.ancestryNumber.toInt > 3
    })

    //Get Aunts and Uncles
    var auntUncleFutures = List[Future[List[Person]]]()
    grandparents.foreach((item) => {
      auntUncleFutures = future {
        FamilySearch.getChildren(token, pid, item)
      } :: auntUncleFutures
    })

    val allAuntUncleFutures = Future.sequence(auntUncleFutures).map {
      lst => lst.foreach(l => grandparentSet.addDescendants(l))
    }
    Await.result(allAuntUncleFutures, Duration(90, TimeUnit.SECONDS))

    // Get cousins
    var cousinFutures = List[Future[List[Person]]]()
    grandparentSet.getDescendants().foreach((item) => {
      cousinFutures = future {
        FamilySearch.getChildren(token, pid, item)
      } :: cousinFutures
    })

    var allCousins = List[Person]()
    val allCousinFutures = Future.sequence(cousinFutures).map {
      auntUncleChildren: List[List[Person]] => {
        auntUncleChildren.foreach((singleAuntUncleChildren: List[Person]) => {
          var cousinsToAdd = singleAuntUncleChildren
          var toRemove = List[Person]()
          singleAuntUncleChildren.foreach((cousin) => {
            val addedCousin = addedCousins.get(cousin.pid).getOrElse(Person(""))
            if (addedCousin.getPid == "") {
              addedCousins += (cousin.getPid() -> cousin)
            }
            else {
              addedCousin.parent.get.altName = " & " + cousin.parent.get.name
              grandparentSet.removeDescendant(cousin.parent.get)
              toRemove = cousin :: toRemove
            }
          })
          toRemove.foreach((person) => cousinsToAdd = cousinsToAdd diff List(person))
          if (cousinsToAdd.size > 0) {
            val auntUncleMatch = getAuntUncleMatch(cousinsToAdd.head, grandparentSet.getDescendants())
            if (auntUncleMatch != null)
              auntUncleMatch.addDescendants(cousinsToAdd)
          }
          allCousins = allCousins ::: cousinsToAdd
        })
      }
    }

    Await.result(allCousinFutures, Duration(90, TimeUnit.SECONDS))

    val cousinList = allCousins.foldLeft(List[Person]())((acc, item) => {
      if (item.pid != pid)
        item :: acc
      else
        acc
    })

    (grandparentSet, cousinList)
  }

  def getAuntsUncles() = Action { implicit request =>
    val token = baseUserForm.bindFromRequest.get.token
    val pid = baseUserForm.bindFromRequest.get.pid

    val treeTuple = getCousinTree(token, pid)
    val grandparentSet = treeTuple._1

    // Remove descendants of Aunts/Uncles unless they are the user in question
    grandparentSet.getDescendants().foreach(p => {
      var containsSelf = false
      p.getDescendants().foreach(d => {
        if (d.pid == pid)
          containsSelf = true
      })
      if (!containsSelf)
        p.clearDescendants()
      else
        p.removeOtherDescendants(pid)
    })

    val auntUncleList = grandparentSet.getDescendants().filter(p => p.getDescendants().size == 0).sortBy(_.getName())
    val json = grandparentSet.toJson

    Ok(views.html.cousins("Aunts & Uncles", auntUncleList.sorted, auntUncleList.size, json.toString()))
  }

  def getCousins() = Action { implicit request =>
    val token = baseUserForm.bindFromRequest.get.token
    val pid = baseUserForm.bindFromRequest.get.pid

    val treeTuple = getCousinTree(token, pid)
    val grandparentSet = treeTuple._1
    var cousinList = List[Person]()

    val gps = Person("", "All Grandparents")
    // Only save aunts/uncles that have descendants
    grandparentSet.getDescendants().filter(p => {
      p.children.size > 0
    }).foreach(p => {
      if (p.containsHighlight)
        p.removeNonHighlighted()
      gps.addDescendant(p)
      cousinList = p.getDescendants() ::: cousinList
    })

    cousinList = cousinList.sortBy(_.getName())
    cousinList = cousinList.filter(p => p.pid != pid)

    val json = gps.toJson

    Ok(views.html.cousins("Cousins", cousinList.sorted, cousinList.size, json.toString()))
  }

  def getAuntUncleMatch(cousin: Person, auntUncleList: List[Person]): Person = {
    var matchedAuntUncle: Person = null
    auntUncleList.foreach(auntUncle => {
      if (auntUncle.pid == cousin.parent.get.pid) {
        matchedAuntUncle = auntUncle
      }
    })
    matchedAuntUncle
  }

  val userForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "nameList" -> text
    )(Cousins.apply)(Cousins.unapply)
  )

}
