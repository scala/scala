//> using jvm 9+
//> using options -Werror

import java.lang.invoke.{MethodHandle, VarHandle}

class C {
  def warn(mh: MethodHandle, vh: VarHandle, c: AnyRef): Unit = {
    mh.invokeExact(c).asInstanceOf[Int]
    mh.invoke(c).asInstanceOf[String]
    vh.getVolatile(c).asInstanceOf[Long]
  }

  def noWarn[T](mh: MethodHandle, vh: VarHandle, c: AnyRef): Unit = {
    (mh.invokeExact(c): Int)
    mh.invoke(c).asInstanceOf[AnyRef]
    mh.invoke(c).asInstanceOf[T]
    (mh.invoke(c): Any).asInstanceOf[Int]
    vh.compareAndSet(c, 1, 2).asInstanceOf[Boolean]
  }
}
