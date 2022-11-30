// javaVersion: 11+
// scalac: -release:11

// this a sequel to t7965. that one only tests MethodHandle. JDK 11 added
// signature polymorphic methods to VarHandle, so let's test that too

// the reason to include `-release:11` is the problem Jason noticed and fixed in
// scala/scala#9930

object Test extends App {
  import java.lang.invoke._
  import scala.runtime.IntRef
  val ref = new scala.runtime.IntRef(0)
  val varHandle = MethodHandles.lookup().in(classOf[IntRef]).findVarHandle(classOf[IntRef], "elem", classOf[Int])
  assert(0 == (varHandle.getAndSet(ref, 1): Int))
  assert(1 == (varHandle.getAndSet(ref, 2): Int))
  assert(2 == ref.elem)
}
