import scala.tools.nsc.interpreter._
import scala.tools.partest.ReplTest

object Test extends ReplTest {
  def code = """
    |import scala.annotation.showAsInfix
    |class &&[T,U]
    |def foo: Int && Boolean = ???
    |def foo: Int && Boolean && String = ???
    |def foo: Int && (Boolean && String) = ???
    |@showAsInfix type Mappy[T, U] = Map[T, U]
    |def foo: Int Mappy (Boolean && String) = ???
    |@showAsInfix(false) class ||[T,U]
    |def foo: Int || Boolean = ???
    |class &:[L, R]
    |def foo: Int &: String = ???
    |def foo: Int &: Boolean &: String = ???
    |def foo: (Int && String) &: Boolean = ???
    |def foo: Int && (Boolean &: String) = ???
    |""".stripMargin
}

