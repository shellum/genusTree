package models

case class Person(pid: String, name: String = "", gender: String = "", parent: Option[Person] = None, highlight: Boolean = false, link: String = "", firstName: String = "", ancestryNumber: String = "", descendancyNumber: String = "") extends Ordered[Person] {
  var children: List[Person] = List[Person]()
  var altName = ""

  override def hashCode(): Int = pid.hashCode()

  override def equals(p1: scala.Any): Boolean = p1.asInstanceOf[Person].pid == pid

  def getName() = name

  def getAltName() = altName

  def getPid() = pid

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

  def toJson: String = {
    var ret = "{\"name\":\"" + name + altName + "\", \"pid\":\"" + pid + "\", \"gender\":\"" + gender + "\", \"highlight\":" + highlight + ", \"children\":[ "
    if (children.size > 0)
      children.foreach((p) => ret += p.toJson + ",")
    ret += "]}"
    ret
  }

  import scala.math.Ordered.orderingToOrdered

  def compare(that: Person): Int = (this.name) compare (that.name)

}