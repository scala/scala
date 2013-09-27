
import scala.tools.partest.SessionTest

// REPL will accrete lines in search of good syntax.
object Test extends SessionTest {
  def session =
""" |Type in expressions to have them evaluated.
    |Type :help for more information.
    |
    |scala> val i = 7
    |i: Int = 7
    |
    |scala> if (i < 7) 8
    |res0: AnyVal = ()
    |
    |scala> else 9
    |res1: Int = 9
    |
    |scala> if (i > 3) 100
    |res2: AnyVal = 100
    |
    |scala> else if (i > 4) 200
    |res3: AnyVal = 100
    |
    |scala> else 300
    |res4: Int = 100
    |
    |scala> "abc"
    |res5: String = abc
    |
    |scala>      .length
    |res6: Int = 3
    |
    |scala> """
}
