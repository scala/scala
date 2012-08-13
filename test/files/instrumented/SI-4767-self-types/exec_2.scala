import scala.tools.partest.instrumented.Instrumentation._
import instrumented._

object Test {
  def main(args: Array[String]) {
    // force predef initialization before profiling
    Predef
    startProfiling()
    // create instances of Foo and Bar, if the inlining did take place you won't see any call to <anonymous class>.debug
    val awithb = new A with B {}
    stopProfiling()
    printStatistics()
  }
}

