import scala.reflect.ClassTag
import scala.tools.partest.instrumented._
import scala.tools.partest.instrumented.Instrumentation._

object Test {
  def main(args: Array[String]): Unit = {
    def doIt = Array[String](null, null, null, null)
    doIt
    startProfiling()
    doIt
    stopProfiling()
    printStatistics()
  }
}
