import scala.tools.partest.instrumented.Instrumentation._

/** We check if classes put in empty package are properly instrumented */
class Foo1 {
  def someMethod = 0
}

/** We check if classes put in `instrumented` package are properly instrumented */
package instrumented {
  class Foo2 {
    def someMethod = 0
  }
}

/** Tests if instrumentation itself works correctly */
object Test {
  def main(args: Array[String]) {
    if (scala.tools.partest.utils.Properties.isAvian) {
      println("!!!TEST SKIPPED!!!")
      println("Instrumentation is not supported on Avian.")
    } else {
      // force predef initialization before profiling
      Predef
      startProfiling()
      val foo1 = new Foo1
      foo1.someMethod
      val foo2 = new instrumented.Foo2
      foo2.someMethod
      // should box the boolean
      println(true)
      stopProfiling()
      printStatistics()
    }
  }
}
