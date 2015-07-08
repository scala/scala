import scala.tools.partest._
import scala.tools.nsc._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -nowarn -Ystop-after:typer"

  override def code = "" // not used

  def code1 = """
package object p1 {
  trait A
  object A
}
  """

  def code2 = """
package object p2 {
  class A
  object A
}
  """

  def code3 = """
package object p3 {
  object A
  trait A
}
  """

  def code4 = """
package object p4 {
  object A
  trait A
}
  """

  def show() {
    val global = newCompiler()
    import global._
    def typecheck(code: String): Unit = {
      val r = new Run
      val sourceFile = newSources(code).head
      global.reporter.reset()
      r.compileSources(sourceFile :: Nil)
      assert(!global.reporter.hasErrors)
    }

    def typecheckTwice(code: String): Unit = {
      typecheck(code)
      typecheck(code)
    }

    // was: illegal cyclic reference involving package ...
    Seq(code1, code2, code3, code4) foreach typecheckTwice
  }
}
