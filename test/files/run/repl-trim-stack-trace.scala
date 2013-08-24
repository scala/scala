
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
  ... 69 elided

scala> def f = throw new Exception("")
f: Nothing

scala> f
java.lang.Exception: 
  at .f(<console>:7)
  ... 69 elided

scala> def f = throw new Exception
f: Nothing

scala> f
java.lang.Exception
  at .f(<console>:7)
  ... 69 elided

scala> """

  // remove the "elided" lines because the frame count is variable
  lazy val elided = """\s+\.{3} (?:\d+) elided""".r
  def filtered(lines: Seq[String]) = lines filter { case elided() => false ; case _ => true }
  override def eval() = filtered(super.eval().toSeq).iterator
  override def expected = filtered(super.expected).toList
}
