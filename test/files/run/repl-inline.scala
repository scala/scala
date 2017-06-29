import scala.tools.nsc._

object Test {
  val testCode =
    """
def callerOfCaller = Thread.currentThread.getStackTrace.drop(2).head.getMethodName
def g = callerOfCaller
def h = g
assert(h == "g", h)
@inline def g = callerOfCaller
def h = g
assert(h == "h", h)
  """

  def main(args: Array[String]) {
    def test(f: Settings => Unit): Unit = {
      val settings = new Settings()
      settings.processArgumentString("-opt:l:inline -opt-inline-from:**")
      f(settings)
      settings.usejavacp.value = true
      val repl = new interpreter.IMain(settings)
      testCode.linesIterator.foreach(repl.interpret(_))
    }
    test(_ => ())
    test(_.Yreplclassbased.value = true)
  }
}
