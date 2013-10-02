import scala.tools.partest.ReplTest

object Test extends ReplTest {
  // My god...it's full of quines
  def code = """
object o { val file = sys.props("partest.cwd") + "/t4671.scala" }
val s = scala.io.Source.fromFile(o.file)
println(s.getLines.mkString("\n"))

val s = scala.io.Source.fromFile(o.file)
println(s.mkString(""))
""".trim
}
