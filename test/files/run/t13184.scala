//> using jvm 16+

import java.lang.invoke.{MethodHandles, VarHandle}

class C {
  @volatile var state: Int = 42
}

object Test {
  // with exact behavior, a call site descriptor that doesn't match the field type throws WrongMethodTypeException
  val h: VarHandle = MethodHandles.privateLookupIn(classOf[C], MethodHandles.lookup()).findVarHandle(classOf[C], "state", Integer.TYPE).withInvokeExactBehavior()

  def main(args: Array[String]): Unit = {
    val c = new C
    assert(h.getVolatile(c).asInstanceOf[Int] == 42)
    assert(h.getVolatile(c).asInstanceOf[Int] + 1 == 43)
    h.setVolatile(c, 7)
    assert((h.getVolatile(c): Int) == 7)
  }
}
