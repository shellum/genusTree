package controllers

import models.{Person, PersonWrites}
import models.PersonWrites.fullWrites
import play.api.Play
import play.api.data.Forms._
import play.api.data._
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.mvc._
import utils.{Timer, Event, FamilySearch}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration
import play.api.Play.current

object Auth extends Controller {

  val EMAIL_KEY: String = Play.current.configuration.getString("email.key").get
  val EMAIL_FROM: String = Play.current.configuration.getString("email.from").get
  val EMAIL_DEST: String = Play.current.configuration.getString("email.dest").get

  def index = Action {
    Event("Homepage")
    Ok(views.html.index(FamilySearch.FAMILYSEARCH_IDENT_URL))
  }

  def privacyPolicy = Action {
    Event("PrivacyPolicy")
    Ok(views.html.privacy())
  }

  def getNameList() = Action { implicit request =>
    val token = nameListForm.bindFromRequest.get.token
    val pid = nameListForm.bindFromRequest.get.pid
    val generations = nameListForm.bindFromRequest.get.generations

    val allPeople = FamilySearch.getAllAncestors(6, pid, token).distinct
    val json = Json.toJson(allPeople).toString()
    Ok(json).as(TEXT)
  }

  def feedback() = Action { implicit request =>
    val from = feedbackForm.bindFromRequest.get.from
    val body = feedbackForm.bindFromRequest.get.body

    sendFeedback(from, body)
    Ok
  }

  def sendFeedback(from: String, body: String) = {
      val timer = Timer("sendFeedback")
      val future = WS.url("https://mandrillapp.com/api/1.0/messages/send.json")
        .post(
          """
            {
                "key": """" + EMAIL_KEY + """",
                "message": {
                    "text": """" + body + """",
                    "subject": "Genustree Feedback: """ +from + """",
                    "from_email": """" + EMAIL_FROM + """",
                    "from_name": "genustree-feedback",
                    "to": [
                        {
                            "email": """" + EMAIL_DEST + """",
                            "name": "Recipient Name",
                            "type": "to"
                        }
                    ]
                }
            }
          """).map { response =>
        timer.logTime()
      }

      Await.result(future, Duration(190, java.util.concurrent.TimeUnit.SECONDS))
  }

  def getAnotherNameList() = Action { implicit reqest =>
    val token = loadingForm.bindFromRequest.get.token
    val pid = loadingForm.bindFromRequest.get.pid
    val pids = loadingForm.bindFromRequest.get.pids
    val pidList = pids.split(",").toList

    Event("LoadOther")

    val allPeople = FamilySearch.getAllDescendants(1, pidList, token).distinct
    val currentUser = getCurrentUserPerson(token)
    val allPeopleAndCurrentUser = (currentUser :: allPeople).distinct
    val json = Json.toJson(allPeopleAndCurrentUser).toString()
    Ok(json).as(TEXT)
  }

  def menu = Action {
    implicit request => {
      val token = userForm.bindFromRequest.get.token
      val pid = userForm.bindFromRequest.get.pid
      val nameList = userForm.bindFromRequest.get.nameList.replaceAll("'", "")

      Ok(views.html.menu(token, pid, nameList))
    }
  }

  def processToken = Action { implicit request =>
    var ret = "";
    val code: String = request.getQueryString("code").getOrElse("")
    val requestMap = Map("grant_type" -> Seq("authorization_code"),
      "code" -> Seq(code),
      "client_id" -> Seq(FamilySearch.FAMILYSEARCH_DEVELOPER_ID))
    val future = WS.url(FamilySearch.FAMILYSEARCH_IDENT_URL + "/cis-web/oauth2/v3/token")
      .post(requestMap).map { response =>
      ret = response.body
    }

    Await.result(future, Duration(190, java.util.concurrent.TimeUnit.SECONDS))

    Event("Login")

    val json = Json.parse(ret)

    (json \ "access_token").asOpt[String] match {
      case None => Redirect(routes.Auth.index())
      case Some(x) =>
        var token: String = (json \ "access_token").toString
        token = "Bearer " + token.replaceAll("\"", "")

        val user = getCurrentUserPerson(token)
        Ok(views.html.loading(token, user.pid))
    }
  }

  def getCurrentUserPerson(token: String): Person = {
    val user = FamilySearch.getCurrentUser(token)
    val j = Json.parse(user)
    val jsarray = j \ "users"
    val personId = (jsarray(0) \ "personId").toString().replace("\"", "")
    val displayName = (jsarray(0) \ "displayName").toString().replace("\"", "")
    Person(personId, displayName)
  }

  def reprocessToken = Action { implicit request =>
    val token: String = request.getQueryString("token").getOrElse("")
    val personId: String = request.getQueryString("pid").getOrElse("")

    Ok(views.html.loading(token, personId))
  }

  val loadingForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "pids" -> text
    )(Loader.apply)(Loader.unapply)
  )


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
  val nameListForm = Form(
    mapping(
      "token" -> text,
      "pid" -> text,
      "generations" -> number
    )(NameListForm.apply)(NameListForm.unapply)
  )

  val feedbackForm = Form(
    mapping(
      "from" -> text,
      "body" -> text
    )(FeedbackForm.apply)(FeedbackForm.unapply)
  )

}

case class BaseForm(token: String, pid: String)
case class FeedbackForm(from: String, body: String)
case class Cousins(token: String, pid: String, nameList: String)
case class Loader(token: String, pid: String, pids: String)

case class NameListForm(token: String, pid: String, generations: Int)
