import scala.tools.partest.instrumented.Instrumentation._
import scala.tools.partest.instrumented.MethodCallTrace
import scala.collection.immutable._

object Test {
  def main(args: Array[String]) {
    val a = HashSet(1,2,3,4,5,6,7,8)
    Predef
    startProfiling()
    val c = a.filter(_<5)
    stopProfiling()
    val stats = getStatistics
    for((method,count)<-stats)
      method match {
        case MethodCallTrace("scala/collection/immutable/HashSet$HashSet1","<init>",_) =>
          println(""+count+" unnecessary instances of HashSet.HashSet1 are being created")
        case MethodCallTrace("scala/runtime/ScalaRunTime$","hash",_) =>
          println(""+count+" unnecessary calls to ScalaRunTime.hash")
        case _ =>
      }
  }
}
