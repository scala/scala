import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = s"""
    |val i = 7
    |val j = 8
    |i * j
    |:save $saveto
  """.stripMargin.trim
  def saveto = testOutput / "session.repl"

  override def show() = {
    super.show()
    Console print saveto.toFile.slurp
  }
}
