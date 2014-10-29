package models

case class SimplePerson(name: String, count: Int) {
  def compare(that: SimplePerson): Int = (this.count) compare (that.count)
}
