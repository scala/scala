import scala.tools.partest.instrumented.Instrumentation._

object Test {
  def main(args: Array[String]) {
    startProfiling()

    // tests optimization in Cleanup for varargs reference arrays
    val a = Array("")

    stopProfiling()
    printStatistics()
  }
}
