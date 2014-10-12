package models

case class Person(name: String, pid: String) {
  var children: List[Person] = List[Person]()
  def addDecendent(person: Person) = {
    if (!children.contains(person))
      children = person :: children
  }
  def getDecendents() = children
}