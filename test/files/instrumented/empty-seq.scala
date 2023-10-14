
import scala.tools.partest.instrumented.Instrumentation._

object Test extends App {
  startProfiling()
  Seq()
  stopProfiling()
  printStatistics()
}
