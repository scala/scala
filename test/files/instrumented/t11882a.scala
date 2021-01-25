import scala.reflect.ClassTag
import scala.tools.partest.instrumented._
import scala.tools.partest.instrumented.Instrumentation._

class OptimusSeq[T]

object OptimusSeq {
  private def unsafeFromAnyArray1[T <: AnyRef](ts: Array[T]): OptimusSeq[T] = null;
  def apply1[T <: AnyRef : ClassTag](p1: T, p2: T, p3: T, p4: T): OptimusSeq[T] = {
    unsafeFromAnyArray1(Array(p1, p2, p3, p4))
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    def doIt = OptimusSeq.apply1[AnyRef](null, null, null, null)
    doIt
    startProfiling()
    doIt
    stopProfiling()
    printStatistics()
  }
}
