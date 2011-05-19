import scala.io.Source

object Test {
  def main(args: Array[String]) {
    val lines = Source.fromString(
      """|
         |This is a file
         |it is split on several lines.
         |
         |isn't it?
         |""".stripMargin).getLines.toList
    println("lines.size = " + lines.size)
    lines.foreach(println)
  }
}
