
import scala.tools.partest.SessionTest

// SI-7740
object Test extends SessionTest {
  def session =
"""Type in expressions to have them evaluated.
Type :help for more information.

scala> def f = throw new Exception("Uh-oh")
f: Nothing

scala> f
java.lang.Exception: Uh-oh
  at .f(<console>:7)

scala> def f = throw new Exception("")
f: Nothing

scala> f
java.lang.Exception: 
  at .f(<console>:7)

scala> def f = throw new Exception
f: Nothing

scala> f
java.lang.Exception
  at .f(<console>:7)

scala> """

}
