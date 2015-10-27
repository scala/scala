import scala.tools.partest.SessionTest
import scala.PartialFunction.{ cond => when }

object Elision {
  val elideMsg = """  ... \d+ elided""".r
}

object Test extends SessionTest {
  import Elision._

  // Filter out the abbreviated stacktrace "... X elided" 
  // because the number seems to differ between versions/platforms/...
  def elided(s: String) = when(s) { case elideMsg() => true }
  override def eval() = super.eval() filterNot elided
  def session =
"""
scala> val 1 = 2
scala.MatchError: 2 (of class java.lang.Integer)

scala> val List(1) = List(1)

scala> :quit
"""
}
