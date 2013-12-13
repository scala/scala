import scala.tools.partest.instrumented.Instrumentation._
import instrumented._

object Test {
  def main(args: Array[String]) {
    if (scala.tools.partest.utils.Properties.isAvian) {
      println("!!!TEST SKIPPED!!!")
      println("Instrumentation is not supported on Avian.")
    } else {
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
}
