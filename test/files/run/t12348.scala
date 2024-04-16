//> using jvm 11+
//> using options -release:11

// this a sequel to t7965. that one only tests MethodHandle. JDK 11 added
// signature polymorphic methods to VarHandle, so let's test that too

// the reason to include `-release:11` is the problem Jason noticed and fixed in
// scala/scala#9930

import java.lang.invoke._
import scala.runtime.IntRef

object Test extends App {
  locally {
    val a = new Array[Object](1)
    val h = MethodHandles.arrayElementVarHandle(a.getClass)
    val r = h.setVolatile(a, 0, "foo") // important: no expected type
  }
  locally {
    val ref = new IntRef(0)
    val varHandle = MethodHandles.lookup().in(classOf[IntRef]).findVarHandle(classOf[IntRef], "elem", classOf[Int])
    assert(0 == (varHandle.getAndSet(ref, 1): Int))
    assert(1 == (varHandle.getAndSet(ref, 2): Int))
    assert(2 == ref.elem)
  }
}
