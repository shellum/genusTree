package models

import play.api.libs.json.{Writes, JsPath}
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Person(pid: String,
                  name: String = "",
                  gender: String = "",
                  parent: Option[Person] = None,
                  highlight: Boolean = false,
                  link: String = "",
                  firstName: String = "",
                  ancestryNumber: String = "",
                  descendancyNumber: String = "",
                  birthYear: String = "",
                  deathYear: String = "",
                  place: String = "") extends Ordered[Person] {


  var children: List[Person] = List[Person]()

  override def hashCode(): Int = pid.hashCode()

  override def equals(p1: scala.Any): Boolean = p1.asInstanceOf[Person].pid == pid

  def getDescendants() = children

  def addDescendant(person: Person) = {
    if (!children.contains(person))
      children = person :: children
  }

  def removeDescendant(person: Person) = {
    children = children.filter(p=>{
      p!=person
    })
  }

  def removeOtherDescendants(pid: String) = {
    children = children.filter(p=>{
      pid == p.pid
    })
  }

  def addDescendants(people: List[Person]) = {
    people.foreach((person) => {
      addDescendant(person)
    })
  }

  def containsHighlight() = {
    children.filter(p=>p.highlight).size>0
  }

  def removeNonHighlighted() = {
    children = children.filter(p=> {
      p.highlight
    })
  }

  def clearDescendants() = {
    children = List[Person]()
  }

  // For single generation graph
  def toJson: String = {
    var ret = "{\"name\":\"" + name + "\", \"pid\":\"" + pid + "\", \"gender\":\"" + gender + "\", \"highlight\":" + highlight + ", \"children\":[ "
    if (children.size > 0)
      children.foreach((p) => ret += p.toJson + ",")
    ret += "]}"
    ret
  }

  import scala.math.Ordered.orderingToOrdered

  def compare(that: Person): Int = (this.name) compare (that.name)

}

case class PersonList(people: List[Person])

object PersonWrites {

//  val listFullWrites: Writes[List[Person]] = (
//    (__ \ "list").write(List[Person])
//    )(unlift(PersonList.unapply))

  implicit val fullWrites: Writes[Person] = (
    (__ \ "pid").write[String] and
      (JsPath \ "name").write[String] and
      (JsPath \ "gender").write[String] and
      (JsPath \ "parent").lazyWriteNullable(Writes.of[Person](fullWrites)) and
      (JsPath \ "highlight").write[Boolean] and
      (JsPath \ "link").write[String] and
      (JsPath \ "firstName").write[String] and
      (JsPath \ "ancestryNumber").write[String] and
      (JsPath \ "descendancyNumber").write[String] and
      (JsPath \ "birthYear").write[String] and
      (JsPath \ "deathYear").write[String] and
      (JsPath \ "place").write[String]
    )(unlift(Person.unapply))

  implicit val fullReads: Reads[Person] = (
    (__ \ "pid").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "gender").read[String] and
      (JsPath \ "parent").lazyReadNullable(Reads.of[Person](fullReads)) and
      (JsPath \ "highlight").read[Boolean] and
      (JsPath \ "link").read[String] and
      (JsPath \ "firstName").read[String] and
      (JsPath \ "ancestryNumber").read[String] and
      (JsPath \ "descendancyNumber").read[String] and
      (JsPath \ "birthYear").read[String] and
      (JsPath \ "deathYear").read[String] and
      (JsPath \ "place").read[String]
    )(Person.apply _)
}