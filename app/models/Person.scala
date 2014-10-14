package models

import play.api.libs.json.{JsPath, Writes, Json}
import play.api.libs.json._
import play.api.libs.functional.syntax._

case class Person(name: String, pid: String, gender: String, parent: Person, highlight: Boolean) {
  var children: List[Person] = List[Person]()
  var altName = ""
  def addDecendent(person: Person) = {
    var alreadyExists = false
    System.out.println("trying to add "+person.name)
    children.foreach((child)=> {
      System.out.println("    comparing "+child.pid+" to " + person.pid + " with " + child.name + " to " + person.name)
      if (child.pid == person.pid)
        alreadyExists = true
    })
    System.out.println("    already there=="+alreadyExists)

    if (!alreadyExists)
      children = person :: children
  }
  def addDecendents(people: List[Person]) = {
    people.foreach((person)=> {
      addDecendent(person)
    })
  }
  def getDecendents() = children


  override def equals(p1: scala.Any): Boolean = p1.asInstanceOf[Person].getPid().equals(pid)

  override def hashCode = {
    var ret: Int = 0
    pid.foreach((c)=> {
      ret * 1000 + c.toInt
    })
    ret
  }
  def getName() = name
  def getPid() = pid

  def toJson : String = {
    var ret = "{\"name\":\""+name+altName+"\", \"pid\":\""+pid+"\", \"gender\":\""+gender+"\", \"highlight\":"+highlight+", \"children\":[ "
    if (children != null)
      children.foreach((p)=> ret += p.toJson + ",")
    ret += "]}"
    ret
  }

}
/*
object Person {
  //implicit val PersonWrites = Json.writes[Person]

  implicit val PersonWrites: Writes[Person] = (
    (__ \ "name").write[String] and
    (__ \ "pid").write[String] and
    (__ \ "children").lazyWrite(Writes.traversableWrites[Person](PersonWrites))
    )(unlift(Person.unapply))

  def unapply(person:Person): Option[(String, String, List[Person])] = Some((person.getName, person.getPid, person.getDecendents()))
}*/