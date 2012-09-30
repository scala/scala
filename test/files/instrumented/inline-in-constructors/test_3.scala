import scala.tools.partest.instrumented.Instrumentation._
import instrumented._

object Test {
  def main(args: Array[String]) {
    // force predef initialization before profiling
    Predef
    MyPredef
    startProfiling()
    val a = new Foo(2)
    val b = new Bar(true)
    stopProfiling()
    printStatistics()
  }
}
