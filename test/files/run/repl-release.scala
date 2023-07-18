import scala.tools.partest.ReplTest
import scala.tools.nsc._
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.shell.ReplReporterImpl

// cf run/repl-inline.scala
object Test extends ReplTest {

  var count = 0

  override def transformSettings(s: Settings) = {
    s.processArguments("-release:8" :: "-opt:inline:**" :: "-Wopt" :: Nil, processAll = true)
    s.Yreplclassbased.value = count > 0
    count += 1
    s
  }

  override def code =
    """
def callerOfCaller = Thread.currentThread.getStackTrace.drop(2).head.getMethodName
@noinline def g = callerOfCaller
@noinline def h = g
assert(h == "g", h)
@inline def g = callerOfCaller
@noinline def h = g
assert(h == "h", h)
  """

  override def show() = {
    super.show()
    super.show()
  }
}
