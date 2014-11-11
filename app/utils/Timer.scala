package utils

case class Timer(what: String) {
  val start = System.currentTimeMillis()

  def logTime() = {
    val end = System.currentTimeMillis()
    val time = end - start
    println(f"measure#$what%s=$time%dms")
  }

}

case class Event(what: String) {
  println(f"count#$what%s=1")
}