
import scala.language.postfixOps

object Test extends App {

 import scala.util.matching.{ Regex, UnanchoredRegex }

 val dateP1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".r.unanchored
 val dateP2 = """(\d\d\d\d)-(\d\d)-(\d\d)""" r ("year", "month", "day") unanchored
 val dateP3 =  new Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""", "year", "month", "day") with UnanchoredRegex

 val yearStr = "2011"
 val dateStr = List(yearStr,"07","15").mkString("-")

  def test(msg: String)(strs: Seq[String]): Unit = println("%40s  %s".format(msg, strs mkString " "))

  test("extract an exact match") {
    val dateP1(y,m,d) = dateStr
    Seq(List(y,m,d).mkString("-"), dateStr)
  }

  test("extract from middle of string") {
    val dateP1(y,m,d) = "Tested on "+dateStr+"."
    Seq(List(y,m,d).mkString("-"), dateStr)
  }

  test("extract from middle of string (P2)") {
    val dateP2(y,m,d) = "Tested on "+dateStr+"."
    Seq(List(y,m,d).mkString("-"), dateStr)
  }

  test("extract from middle of string (P3)") {
    val dateP2(y,m,d) = "Tested on "+dateStr+"."
    Seq(List(y,m,d).mkString("-"), dateStr)
  }

  def copyright(in: String): String = in match {
    case dateP1(year, month, day) => "Copyright "+year
    case _                        => "No copyright"
  }

  test("copyright example has date") {
    Seq(copyright("Date of this document: "+dateStr), "Copyright "+yearStr)
  }

  test("copyright example missing date") {
    Seq(copyright("Date of this document: unknown"), "No copyright")
  }
}
