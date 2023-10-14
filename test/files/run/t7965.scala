// Test that scala doesn't apply boxing or varargs conversions to the
// @PolymorphicSignature magical methods, MethodHandle#{invoke, invokeExact}

import java.lang.invoke._

object O {
  private def foo = "foo"
  private def bar(x: Int): Int = -x
  private def baz(x: Box): Unit = x.a = "present"
  val lookup = MethodHandles.lookup
}

class Box(var a: Any)

object Test extends App {
  def lookup(name: String, params: Array[Class[_]], ret: Class[_]) = {
    val mt = MethodType.methodType(ret, params)
    O.lookup.findVirtual(O.getClass, name, mt)
  }
  val fooResult = (lookup("foo", Array(), classOf[String]).invokeExact(O): String)
  assert(fooResult == "foo")

  val barResult = (lookup("bar", Array(classOf[Int]), classOf[Int]).invokeExact(O, 42): Int)
  assert(barResult == -42)

  val box = new Box(null)
  (lookup("baz", Array(classOf[Box]), Void.TYPE).invokeExact(O, box) : Unit)
  assert(box.a == "present")

  // Note: Application in statement position in a block in Java also infers return type of Unit,
  // but we don't support that, ascribe the type to Unit as above.
  // as done in Java.
  // lookup("baz", Array(classOf[Box]), Void.TYPE).invokeExact(O, box)

}
