object Test extends App {

 import scala.util.matching.{Regex, NoImplicitAnchors}

 val dateP1 = """(\d\d\d\d)-(\d\d)-(\d\d)""".qr
 val dateP2 = """(\d\d\d\d)-(\d\d)-(\d\d)""" qr ("year", "month", "day")
 val dateP3 =  new Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""", "year", "month", "day") with NoImplicitAnchors

 val yearStr = "2011"
 val dateStr = List(yearStr,"07","15").mkString("-")

  def test(msg: String)(body: => Boolean): Unit = assert(body, msg)

  test("extract an exact match") {
    val dateP1(y,m,d) = dateStr
    List(y,m,d).mkString("-") == dateStr
  }

  test("extract from middle of string") {
    val dateP1(y,m,d) = "Tested on "+dateStr+"."
    List(y,m,d).mkString("-") == dateStr
  }

  test("extract from middle of string (P2)") {
    val dateP2(y,m,d) = "Tested on "+dateStr+"."
    List(y,m,d).mkString("-") == dateStr
  }

  test("extract from middle of string (P3)") {
    val dateP2(y,m,d) = "Tested on "+dateStr+"."
    List(y,m,d).mkString("-") == dateStr
  }

  def copyright(in: String): String = in match {
    case dateP1(year, month, day) => "Copyright "+year
    case _                        => "No copyright"
  }

  test("copyright example has date") {
    copyright("Date of this document: "+dateStr) == "Copyright "+yearStr
  }

  test("copyright example missing date") {
    copyright("Date of this document: unknown") == "No copyright"
  }
}
