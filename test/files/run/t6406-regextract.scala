
object Test extends App {
  import util.matching._
  import Regex._

  val r = "(\\d+)".r
  val q = """(\d)""".r
  val ns = List("1,2","x","3,4")
  val u = r.unanchored

  val is = ns collect { case u(x) => x } map { case r(x) => x }
  println(is)
  // Match from same pattern
  val js = (ns map { u findFirstMatchIn _ }).flatten map { case r(x) => x }
  println(js)
  // Match not from same pattern
  val ks = (ns map { q findFirstMatchIn _ }).flatten map { case r(x) => x }
  println(ks)

  val t = "Last modified 2011-07-15"
  val p1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".r
  val y1: Option[String] = for {
    p1(year, month, day) <- p1 findFirstIn t
  } yield year
  val y2: Option[String] = for {
    p1(year, month, day) <- p1 findFirstMatchIn t
  } yield year
  println(s"$y1 $y2")

}
