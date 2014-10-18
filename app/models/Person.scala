package models

case class Person(name: String, pid: String, gender: String, parent: Option[Person], highlight: Boolean = false) {
  var children: List[Person] = List[Person]()
  var altName = ""

  override def hashCode(): Int = super.hashCode()

  override def equals(p1: scala.Any): Boolean = p1.asInstanceOf[Person].pid == pid

  def getName() = name

  def getPid() = pid

  def getDecendents() = children

  def addDecendent(person: Person) = {
    if (!children.contains(person))
      children = person :: children
  }

  def addDecendents(people: List[Person]) = {
    people.foreach((person) => {
      addDecendent(person)
    })
  }

  def toJson: String = {
    var ret = "{\"name\":\"" + name + altName + "\", \"pid\":\"" + pid + "\", \"gender\":\"" + gender + "\", \"highlight\":" + highlight + ", \"children\":[ "
    if (children.size > 0)
      children.foreach((p) => ret += p.toJson + ",")
    ret += "]}"
    ret
  }

}