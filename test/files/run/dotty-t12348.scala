//> using jvm 11+
//> using options -release:11

import java.lang.invoke._
import scala.runtime.IntRef

object Test {
  def main(args: Array[String]): Unit = {
    val ref = new scala.runtime.IntRef(0)
    val varHandle = MethodHandles.lookup()
      .in(classOf[IntRef])
      .findVarHandle(classOf[IntRef], "elem", classOf[Int])
    assert(0 == (varHandle.getAndSet(ref, 1): Int))
    assert(1 == (varHandle.getAndSet(ref, 2): Int))
    assert(2 == ref.elem)

    assert((()) == (varHandle.set(ref, 3): Any))
    assert(3 == (varHandle.get(ref): Int))

    assert(true == (varHandle.compareAndSet(ref, 3, 4): Any))
    assert(4 == (varHandle.get(ref): Int))
  }
}
