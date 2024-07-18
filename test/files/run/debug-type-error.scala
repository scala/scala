//> using filter (\s*)at(.*)

import scala.tools.partest._

object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Vdebug-type-error"

  def code: String = ""

  def noSuchType: String = """
object Example
{
  val a: org.dummy.Dummy = ???
}
  """

  def show(): Unit = {
    val global = newCompiler()

    def run(code: String): Unit =
      compileString(global)(code.trim)

    run(noSuchType)
  }
}
