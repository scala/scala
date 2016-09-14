import scala.tools.nsc.interpreter._
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.annotation.showAsInfix
    |class &&[T,U]
    |def foo: Int && Boolean = ???
    |@showAsInfix class ||[T,U]
    |def foo: Int || Boolean = ???
    |@showAsInfix class &&[T, U]
    |def foo: Int && Boolean && String = ???
    |def foo: Int && (Boolean && String) = ???
    |@showAsInfix type Mappy[T, U] = Map[T, U]
    |def foo: Int Mappy (Boolean && String) = ???
    |""".stripMargin
}

