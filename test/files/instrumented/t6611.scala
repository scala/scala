import scala.tools.partest.instrumented.Instrumentation._

object Test {
  def main(args: Array[String]) {
    startProfiling()

    // tests optimization in Cleanup for varargs reference arrays
    Array("")


    Array(true)
    Array(true, false)
    Array(1: Byte)
    Array(1: Byte, 2: Byte)
    Array(1: Short)
    Array(1: Short, 2: Short)
    Array(1)
    Array(1, 2)
    Array(1L)
    Array(1L, 2L)
    Array(1d)
    Array(1d, 2d)
    Array(1f)
    Array(1f, 2f)

    /* Not currently optimized:
    Array[Int](1, 2) etc
    Array(())
    Array((), ())
    */

    stopProfiling()
    printStatistics()
  }
}
