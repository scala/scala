
import scala.tools.partest.SessionTest

// Detected repl transcript paste: ctrl-D to finish.
object Test extends SessionTest {
  def session =
""" |Type in expressions to have them evaluated.
    |Type :help for more information.
    |
    |scala> @deprecated(message="Please don't do that.", since="Time began.") def depp = "john"
    |depp: String
    |
    |scala> def a = depp
    |warning: there was one deprecation warning; re-run with -deprecation for details
    |a: String
    |
    |scala> :settings +deprecation
    |
    |scala> def b = depp
    |<console>:8: warning: method depp is deprecated: Please don't do that.
    |       def b = depp
    |               ^
    |b: String
    |
    |scala> """
}
