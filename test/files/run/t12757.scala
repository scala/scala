
import scala.tools.partest.DirectTest

object Test extends DirectTest {
  def header = """|object Test extends App {
                  |  val myStrings: List[String] = List(""".stripMargin.linesIterator
  def footer = """|  )
                  |  println(myStrings.mkString(","))
                  |}""".stripMargin.linesIterator
  def values = Iterator.tabulate(4000)(i => s"    \"$i\",")
  def code   = (header ++ values ++ footer).mkString("\n")

  override def extraSettings: String = "-usejavacp -J-Xms256k"

  def show() = assert(compile())
}
