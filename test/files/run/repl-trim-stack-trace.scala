
import scala.tools.partest.{ SessionTest, Welcoming }

// SI-7740
object Test extends SessionTest with Welcoming {
  def session =
"""Welcome to Scala
Type in expressions for evaluation. Or try :help.

scala> def f = throw new Exception("Uh-oh")
f: Nothing

scala> f
java.lang.Exception: Uh-oh
  at .f(<console>:11)
  ... 69 elided

scala> def f = throw new Exception("")
f: Nothing

scala> f
java.lang.Exception:
  at .f(<console>:11)
  ... 69 elided

scala> def f = throw new Exception
f: Nothing

scala> f
java.lang.Exception
  at .f(<console>:11)
  ... 69 elided

scala> :quit"""

  // normalize the "elided" lines because the frame count depends on test context
  lazy val elided = """(\s+\.{3} )\d+( elided)""".r
  override def normalize(line: String) = line match {
    case elided(ellipsis, suffix) => s"$ellipsis???$suffix"
    case s                        => s
  }
  override def expected = super.expected map normalize
}
