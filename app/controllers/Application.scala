package controllers

import java.util.concurrent.TimeUnit

import models.{SimplePerson, Person}
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
  val FAMILYSEARCH_IDENT_URL: String = Play.current.configuration.getString("familysearch.ident.url").get

  def index = Action {
    Ok(views.html.index(FAMILYSEARCH_IDENT_URL))
  }

  def privacyPolicy = Action {
    Ok(views.html.privacy())
  }

  def nameCloud = Action { implicit request =>
    val token = nameCloudForm.bindFromRequest.get.token
    val pid = nameCloudForm.bindFromRequest.get.pid
    val generations = nameCloudForm.bindFromRequest.get.generations

   // var allPeople = List[Person]()

    var addedCousins: Map[String, Person] = Map[String, Person]()
    val grandparentSet = Person("Grandparent Set", "", "", None, false)

    var allPeople:List[Person]= List[Person]()
    System.out.println("starting walk up tree")
    //Walk up the tree
    //Start with self
    var parents: List[Person] = List(Person("doesn't matter", pid, "", None))
    (1 to generations-1).foreach(p=>parents = parents.foldLeft(List[Person]())((acc, item) => getParents(token, item) ::: acc))



    allPeople =  parents:::allPeople

    //Walk back down the tree

    System.out.println("done with walk up tree")

    (1 to generations-1).foreach(something => {
      var nextGenToFollow = List[Person]()
      //Go down one generation
      var auntUncleFutures = List[Future[List[Person]]]()
      parents.foreach((item) => {
        auntUncleFutures = future {
          getChildren(token, pid, item)
        } :: auntUncleFutures
      })

      val allAuntUncleFutures = Future.sequence(auntUncleFutures).map {
        lst => lst.foreach(l => allPeople = {
          nextGenToFollow = l
          allPeople ::: l
        })
      }
      Await.result(allAuntUncleFutures, Duration(50, TimeUnit.SECONDS))
      parents = nextGenToFollow
      System.out.println("done with going down one generation")
    })


    var json = "["
    var nameMap = Map[String, Int]()
    allPeople.distinct.foreach(p=>{
      val allButLastName = p.name.split(" ")
      (0 to allButLastName.length - 2).foreach(index => {
        val part = allButLastName(index)
        val count = nameMap.get(part.toUpperCase)
        if (part != "" && part.length > 1 && !excludedNames.contains(part.toLowerCase()))
        count match {
          case Some(x) => nameMap = nameMap + (part.toUpperCase() -> (nameMap.get(part.toUpperCase()).get + 5))
          case _ => nameMap = nameMap + (part.toUpperCase() -> 5)
        }
      })
    })



    var nameCount = 0
    var simplePersonList = List[SimplePerson]()

    nameMap.foreach(p => {
      simplePersonList = SimplePerson(p._1,p._2) :: simplePersonList
    })

    val sortedSimpleList = simplePersonList.sortWith(_.count > _.count)

    var maxSize = 0
    sortedSimpleList.foreach(p=> {
      if (p.count > maxSize)
        maxSize = p.count
    })

    sortedSimpleList.foreach(p=> {
      nameCount = nameCount + 1
      if (nameCount < 100)
      json = json + "{name:\"" + p.name + "\",size:"+((p.count * 40) / maxSize)+"},"
    })

    if (nameCount < 100)
    do {
      nameMap.foreach(p => {
        nameCount = nameCount + 1
        json = json + "{name:\"" + p._1 + "\",size:10},"
      })
    } while(nameCount < 150)
  /*  nameMap.foreach(p => {
      json = json + "{name:\"" + p._1 + "\",size:5},"
    })
    nameMap.foreach(p => {
      json = json + "{name:\"" + p._1 + "\",size:5},"
    })
    nameMap.foreach(p => {
      json = json + "{name:\"" + p._1 + "\",size:5},"
    })
    nameMap.foreach(p => {
      json = json + "{name:\"" + p._1 + "\",size:5},"
    })*/

    json = json.substring(0,json.length-1)
    json = json + "]"

    Ok(views.html.namecloud(json))
  }

  def excludedNames = List("stillborn","stilborn","still")

  def search = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid

      Ok(views.html.search(token, pid))
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

  def getCousinTree(token: String, pid: String): (Person, List[Person]) = {
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

    val allAuntUncleFutures = Future.sequence(auntUncleFutures).map {
      lst => lst.foreach(l => grandparentSet.addDescendants(l))
    }
    Await.result(allAuntUncleFutures, Duration(50, TimeUnit.SECONDS))

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
            getAuntUncleMatch(cousinsToAdd.head, grandparentSet.getDescendants()).addDescendants(cousinsToAdd)
          }
          allCousins = allCousins ::: cousinsToAdd
        })
      }
    }

    Await.result(allCousinFutures, Duration(50, TimeUnit.SECONDS))

    val cousinList = allCousins.foldLeft(List[Person]())((acc, item) => {
      if (item.pid != pid)
        item :: acc
      else
        acc
    })

    (grandparentSet, cousinList)
  }

  def getAuntsUncles() = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid

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

    val auntUncleList = grandparentSet.getDescendants().filter(p=>p.getDescendants().size==0).sortBy(_.getName())

    val json = grandparentSet.toJson

    Ok(views.html.cousins("Aunts & Uncles",auntUncleList.sorted, auntUncleList.size, json.toString()))
  }


  def getCousins() = Action { implicit request =>
    val token = userForm.bindFromRequest.get.token
    val pid = userForm.bindFromRequest.get.pid

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
    cousinList = cousinList.filter(p=>p.pid != pid)

    val json = gps.toJson

    Ok(views.html.cousins("Cousins",cousinList.sorted, cousinList.size, json.toString()))
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
            val link = (item \ "links" \ "person" \ "href").toString().replaceAll("\"", "")
            val person = Person(name, id, gender, None, link = link)
            person :: acc
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
    Await.result(future, Duration(50, java.util.concurrent.TimeUnit.SECONDS))
    ret
  }

  val userForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text
    )(Cousins.apply)(Cousins.unapply)
  )
  val nameCloudForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "generations" -> number
    )(NameCloudParams.apply)(NameCloudParams.unapply)
  )

}

case class Cousins(token: String, pid: String)
case class NameCloudParams(token: String, pid: String, generations: Int)
