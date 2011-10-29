import scala.tools.nsc.interpreter._
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  class M[@specialized T] { }

  def code = """
  |import scala.reflect.Code
  |def codeOf[A](code: Code[A]) = code
  |codeOf((x: Iterable[_]) => throw new Exception)
  """.stripMargin
}
