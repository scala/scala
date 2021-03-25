import scala.tools.partest.instrumented.Instrumentation._

object Test {
  def main(args: Array[String]): Unit = {
    startProfiling()

    // to optimized
    val x = Array[Double](1)
    val y = Array[Double](1.0)

    // Currently correctly optimized
    val i                = Array(1.0)
    val j: Array[Double] = Array(1)

    //others case
    val a: Array[Double] = Array[Double](1.0)
    val b: Array[Double] = Array[Double](1)
    val c: Array[Double] = Array[Double](1: Double)
    val d: Array[Double] = Array(1: Double)
    val e                = Array(1: Double)
    val f                = Array(1: Int)
    val g                = Array[Int](1)
    val h                = Array(1)
    val k                = Array[Unit](())

    stopProfiling()
    printStatistics()
  }
}
