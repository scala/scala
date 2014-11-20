// Test that scala doesn't apply boxing or varargs conversions to the
// @PolymorphicSignature magical methods, MethodHandle#{invoke, invokeExact}
object Test {
  val code = """

object O {
  private def foo = "foo"
  private def bar(x: Int): Int = -x
  private def baz(x: Box): Unit = x.a = "present"
  val lookup = java.lang.invoke.MethodHandles.lookup
}

import java.lang.invoke._
class Box(var a: Any)

object Test {
  def main(args: Array[String]): Unit = {
    def lookup(name: String, params: Array[Class[_]], ret: Class[_]) = {
      val mt = MethodType.methodType(ret, params)
      O.lookup.findVirtual(O.getClass, name, mt)
    }
    val fooResult = (lookup("foo", Array(), classOf[String]).invokeExact(O): Int)
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
    ()
  }
}

"""
  def main(args: Array[String]): Unit = {
    if (util.Properties.isJavaAtLeast("1.7")) test()
  }

  def test() {
    import scala.reflect.runtime._
    import scala.tools.reflect.ToolBox

    val m = currentMirror
    val tb = m.mkToolBox()
    import tb._
    eval(parse(code))
  }
}
