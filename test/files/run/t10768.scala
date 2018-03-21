import scala.tools.nsc.interpreter._
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |type Id[T] = T
    |def foo(x: Int): Id[x.type] = x
    |foo(1)
    |""".stripMargin
}
