package controllers

import java.util.concurrent.TimeUnit

import models.{ColorScheme, Person, SimplePerson}
import org.apache.commons.lang3.{StringEscapeUtils, StringUtils}
import play.api.Play
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.{JsUndefined, JsObject, Json}
import play.api.libs.ws.WS
import play.api.mvc._

import scala.collection.SortedMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration

object Application extends Controller {

  val FAMILYSEARCH_DEVELOPER_ID: String = Play.current.configuration.getString("familysearch.developer.id").get
  val FAMILYSEARCH_SERVER_URL: String = Play.current.configuration.getString("familysearch.server.url").get
  val FAMILYSEARCH_IDENT_URL: String = Play.current.configuration.getString("familysearch.ident.url").get

  def index = Action {
    Ok(views.html.index(FAMILYSEARCH_IDENT_URL))
  }

  def privacyPolicy = Action {
    Ok(views.html.privacy())
  }

  def getLastDescendent(map: Map[String, Person], num: Int, generation: Int): (Person,Int) = {
    val a = map.get(num.toString)
    a match {
      case None =>
        getLastDescendent(map, num/2, generation-1)
      case Some(p) =>
        (p, generation)
    }
  }

  def getDescendants(token: String, pid: String, generations: Int): List[Person] = {
    var allPeople: List[Person] = List()
    var nextGenToFollow: List[Person] = List(Person("Start",pid,"",None))
    (1 to (generations+1)/2).reverse.foreach(i=> {
      val generationsToGet = i match {
        case 1 => 1
        case _ => 2
      }
      var descendantFutures: List[Future[List[Person]]] = List()
      nextGenToFollow.foreach(p=> {
        descendantFutures = future {
          getAncestors(token, p.pid, generationsToGet, "/platform/tree/descendancy")
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

  def getAllPeople(generations: Int, pid: String, token: String): List[Person] = {
    var allPeople: List[Person] = List[Person]()
    System.out.println("starting walk up tree")
    //Walk up the tree
    //Start with self
    var parents: List[Person] = List(Person("doesn't matter", pid, "", None))
    var ancestors: List[Person] = getAncestors(token, pid, generations,"/platform/tree/ancestry")

    allPeople = ancestors.distinct

    var ancestoryNumberToPersonMap: Map[String, Person] = Map()
    ancestors.foreach(p=> {
      ancestoryNumberToPersonMap += p.ancestryNumber -> p
    })

    var descendantGenerations = 1
    var descendantFutures: List[Future[List[Person]]] = List()
    allPeople.foreach(p=> {
      descendantFutures = future {getDescendants(token, p.pid, 2)} :: descendantFutures
    })
//    var descendantGenerations = 1
//    var descendantFutures: List[Future[List[Person]]] = List()
//    (Math.pow(2,generations).toInt to Math.pow(2,(generations+1)).toInt - 1).foreach(i=> {
//      var personAndGeneration = getLastDescendent(ancestoryNumberToPersonMap,i,generations)
//      println("spawn for "+personAndGeneration._1.pid)
//      descendantFutures = future {getDescendants(token, personAndGeneration._1.pid, personAndGeneration._2)} :: descendantFutures
//    })

    val f = Future.sequence(descendantFutures).map(futureList => futureList.foreach(singleFuture => allPeople = (singleFuture ::: allPeople).distinct))
    Await.result(f, Duration(90, TimeUnit.SECONDS))
    //Walk back down the tree

    System.out.println("done with walk up tree")

    allPeople.distinct
  }

  def getNameList() = Action { implicit request =>
    val token = nameListForm.bindFromRequest.get.token
    val pid = nameListForm.bindFromRequest.get.pid
    val generations = nameListForm.bindFromRequest.get.generations

    var allPeople = getAllPeople(5, pid, token).distinct
    var json = "["
    allPeople.distinct.foreach(p => {
      json = json + "{name:\"" + p.name + "\",pid:\"" + p.pid + "\"},"
    })
    json = json.substring(0, json.length - 1)
    json = json + "]"
    json = json.replace("'","")
    Ok(json).as(TEXT)
  }

  def nameCloud = Action { implicit request =>
    val token = nameCloudForm.bindFromRequest.get.token
    val pid = nameCloudForm.bindFromRequest.get.pid
    val generations = nameCloudForm.bindFromRequest.get.generations
    val unsanitizedFont = nameCloudForm.bindFromRequest.get.font
    val colorScheme = ColorScheme(nameCloudForm.bindFromRequest.get.colorScheme)

    val font = StringEscapeUtils.escapeHtml4(StringEscapeUtils.escapeEcmaScript(unsanitizedFont))

    // var allPeople = List[Person]()

    var addedCousins: Map[String, Person] = Map[String, Person]()
    val grandparentSet = Person("Grandparent Set", "", "", None, false)
    var allPeople = getAllPeople(generations, pid, token)


    var partToNamesMap = Map[String, List[String]]()

    var json = "["
    var nameMap = Map[String, Int]()
    allPeople.distinct.foreach(p => {
      val allButLastName = p.name.split(" ")
      (0 to allButLastName.length - 2).foreach(index => {
        val part = allButLastName(index)
        val upperCasePart = part.toUpperCase()
        val count = nameMap.get(upperCasePart)

        partToNamesMap.get(upperCasePart) match {
          case Some(x) => partToNamesMap += upperCasePart -> ((p.name + " " + p.pid) :: x)
          case _ => partToNamesMap += upperCasePart -> List(p.name + " " + p.pid)
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
      //if (nameCount < 100) {
        var nameSize = ((p.count * 45) / maxSize)
        val smallestSize = 13 - (sortedSimpleList.size / 100)
        if (nameSize < smallestSize) nameSize = smallestSize
        json = json + "{name:\"" + p.name + "\",size:" + nameSize + "},"
      //}
    })

    if (nameCount < 100)
      do {
        sortedSimpleList.foreach(p => {
          nameCount = nameCount + 1
          json = json + "{name:\"" + p.name + "\",size:10},"
        })
      } while (nameCount < 150)

    var i = 50
    if (sortedSimpleList.size < 50) i = sortedSimpleList.size - 1
    (1 to i).foreach(i=> {
      json = json + "{name:\"" + sortedSimpleList(i).name + "\",size:10},"
    })

    json = json.substring(0, json.length - 1)
    json = json + "]"

    val sortedPartToNamesMap = partToNamesMap.toList.sortBy(_._2.size).reverse
    Ok(views.html.namecloud(json, sortedPartToNamesMap, font, colorScheme))
  }

  def excludedNames = List("stillborn", "stilborn", "still", "mr.", "mr", "miss", "miss.", "mrs", "mrs.")

  def search = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid
      val nameList = userForm.bindFromRequest.get.nameList

      Ok(views.html.search(token, pid, nameList))
    }
  }
  def menu = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid
      val nameList = userForm.bindFromRequest.get.nameList

      Ok(views.html.menu(token, pid, nameList))
    }
  }

  def nameCloudDetails = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid

      Ok(views.html.nameclouddetails(token, pid))
    }
  }

  def processToken = Action { implicit request =>

    var ret = "";
    val code: String = request.getQueryString("code").getOrElse("")
    val requestMap = Map("grant_type" -> Seq("authorization_code"),
      "code" -> Seq(code),
      "client_id" -> Seq(FAMILYSEARCH_DEVELOPER_ID))
    val future = WS.url(FAMILYSEARCH_IDENT_URL + "/cis-web/oauth2/v3/token")
      .post(requestMap).map { response =>
      ret = response.body
    }

    Await.result(future, Duration(30, java.util.concurrent.TimeUnit.SECONDS))
    val json = Json.parse(ret)

    (json \ "access_token").asOpt[String] match {
      case None => Redirect(routes.Application.index())
      case Some(x) =>
        var token: String = (json \ "access_token").toString
        token = "Bearer " + token.replaceAll("\"", "")

        val user = getCurrentUser(token)
        val j = Json.parse(user)
        val jsarray = j \ "users"
        val personId = (jsarray(0) \ "personId").toString().replace("\"", "")
        val displayName = (jsarray(0) \ "displayName").toString().replace("\"", "")
        Ok(views.html.loading(token, personId))
    }


  }

  def getCurrentUser(token: String) = {
    var ret = ""
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/users/current")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      ret = response.body
    }

    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getCousinTree(token: String, pid: String): (Person, List[Person]) = {
    var addedCousins: Map[String, Person] = Map[String, Person]()
    val grandparentSet = Person("Grandparent Set", "", "", None, false)

    //Walk up the tree
    var start = System.currentTimeMillis()/1000

    var grandparents = getAncestors(token, pid, 2,"/platform/tree/ancestry").filter(p=> {!p.ancestryNumber.contains("S") && p.ancestryNumber.toInt>3})

    var mid = System.currentTimeMillis()/1000
    //Walk back down the tree

    //Get Aunts and Uncles
    var auntUncleFutures = List[Future[List[Person]]]()
    grandparents.foreach((item) => {
      auntUncleFutures = future {
        getChildren(token, pid, item)
      } :: auntUncleFutures
    })

    val allAuntUncleFutures = Future.sequence(auntUncleFutures).map {
      lst => lst.foreach(l => grandparentSet.addDescendants(l))
    }
    Await.result(allAuntUncleFutures, Duration(90, TimeUnit.SECONDS))

    var mid2 = System.currentTimeMillis()/1000

    // Get cousins
    var cousinFutures = List[Future[List[Person]]]()
    grandparentSet.getDescendants().foreach((item) => {
      cousinFutures = future {
        getChildren(token, pid, item)
      } :: cousinFutures
    })

    var allCousins = List[Person]()
    val allCousinFutures = Future.sequence(cousinFutures).map {
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
    var end = System.currentTimeMillis()/1000

    println("Times: "+ (mid-start) + ", " + (mid2 - mid) + ", " + (end - mid2))

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

    val gps = Person("All Grandparents", "", "", None)
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
            val link = (item \ "links" \ "person" \ "href").toString().replaceAll("\"", "")
            val person = Person(name, id, gender, None, link = link)
            person :: acc
          })
      }
    }
    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getChildren(token: String, selfPid: String, parent: Person): List[Person] = {
    var ret: List[Person] = List[Person]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/persons/" + parent.getPid + "/children")
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      val body = response.body
      body match {
        case "" =>
        case _ =>
          val json = Json.parse(body)
          val jsarray = json \ "persons"
          ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
            val id = (item \ "id").toString().replaceAll("\"", "")
            val name = (item \ "display" \ "name").toString().replaceAll("\"", "")
            val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
            val link = (item \ "links" \ "person" \ "href").toString().replaceAll("\"", "")
            val firstName = ((item \ "names")(0))
            val firsts = (firstName \ "nameForms")(0)
            val fir = (firsts \ "parts")
            val firstNamez = (fir(0) \ "value").toString().replaceAll("\"", "").split(" ")(0)
            val highlight = id.equals(selfPid)
            val person = Person(name, id, gender, Some[Person](parent), highlight = highlight, link = link, firstName = firstNamez)
            person :: acc
          })
      }
    }
    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getNameToIdMap(token: String, selfPid: String): List[Person] = {
    var ret: List[Person] = List[Person]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + "/platform/tree/ancestry?person=" + selfPid)
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      val body = response.body
      body match {
        case "" =>
        case _ =>
          val json = Json.parse(body)
          val jsarray = json \ "persons"
          ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
            val id = (item \ "id").toString().replaceAll("\"", "")
            val name = (item \ "display" \ "name").toString().replaceAll("\"", "")
            val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
            val link = (item \ "links" \ "person" \ "href").toString().replaceAll("\"", "")
            val firstName = ((item \ "names")(0))
            val firsts = (firstName \ "nameForms")(0)
            val fir = (firsts \ "parts")
            val firstNamez = (fir(0) \ "value").toString().replaceAll("\"", "").split(" ")(0)
            acc
          })
      }
    }
    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  def getAncestors(token: String, selfPid: String, generations: Int, url: String): List[Person] = {
    var ret: List[Person] = List[Person]()
    val future = WS.url(FAMILYSEARCH_SERVER_URL + url +"?person=" + selfPid+"&generations="+generations)
      .withHeaders(("Accept", "application/x-fs-v1+json"), ("Authorization", token))
      .get().map { response =>
      val body = response.body
      body match {
        case "" =>
        case _ =>
          val json = Json.parse(body)
          val jsarray = json \ "persons"
         // println(jsarray.toString())
          ret = jsarray.as[List[JsObject]].foldLeft(List[Person]())((acc: List[Person], item: JsObject) => {
            val id = (item \ "id").toString().replaceAll("\"", "")
            val name = (item \ "display" \ "name").toString().replaceAll("\"", "").replace("\\","")
            val ancesteryNumber = (item \ "display" \ "ascendancyNumber")
            var ancesteryNumberStr = ""
            if (ancesteryNumber != JsUndefined) ancesteryNumberStr = ancesteryNumber.toString().replaceAll("\"", "")
            val descendancyNumber = (item \ "display" \ "descendancyNumber")
            var descendancyNumberStr = ""
            if (descendancyNumber != JsUndefined) descendancyNumberStr = descendancyNumber.toString().replaceAll("\"", "")
            val gender = (item \ "gender" \ "type").toString().replaceAll("\"", "").replace("http://gedcomx.org/", "")
            val link = (item \ "links" \ "person" \ "href").toString().replaceAll("\"", "")
            val firstName = ((item \ "names")(0))
            val firsts = (firstName \ "nameForms")(0)
            val fir = (firsts \ "parts")
            val firstNamez = (fir(0) \ "value").toString().replaceAll("\"", "").replace("\\","").split(" ")(0)
         //   println("FOUND " + name + " at " + generations)
            val person = Person(name, id, gender, None, link = link, firstName = firstNamez, ancestryNumber = ancesteryNumberStr, descendancyNumber = descendancyNumberStr)
            person :: acc
          })
      }
    }
    Await.result(future, Duration(90, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }


  val userForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "nameList" -> text
    )(Cousins.apply)(Cousins.unapply)
  )
  val baseUserForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text
    )(BaseForm.apply)(BaseForm.unapply)
  )
  val nameCloudForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "generations" -> number,
      "font" -> text,
      "colorScheme" -> number
    )(NameCloudParams.apply)(NameCloudParams.unapply)
  )
  val nameListForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "generations" -> number
    )(NameListForm.apply)(NameListForm.unapply)
  )

}

case class BaseForm(token: String, pid: String)

case class Cousins(token: String, pid: String, nameList: String)

case class NameListForm(token: String, pid: String, generations: Int)
case class NameCloudParams(token: String, pid: String, generations: Int, font: String, colorScheme: Int)
