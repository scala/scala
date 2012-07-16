import scala.tools.partest.instrumented.Instrumentation._

/** Tests if instrumentation itself works correctly */
object Test {
  def main(args: Array[String]) {
    // force predef initialization before profiling
    Predef
    startProfiling()
    // should box the boolean
    println(true)
    stopProfiling()
    printStatistics()
  }
}
