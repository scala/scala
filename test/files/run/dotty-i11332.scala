import java.lang.invoke._, MethodType.methodType

class Foo {
  def neg(x: Int): Int = -x
  def rev(s: String): String = s.reverse
  def over(l: Long): String = "long"
  def over(i: Int): String  = "int"
  def unit(s: String): Unit = ()
  def obj(s: String): Object = s
  def void(v: Void): String = "void"
}

object Test {
  def main(args: Array[String]): Unit = {
    val l = MethodHandles.lookup()
    val self    = new Foo()
    val mhNeg   = l.findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
    val mhRev   = l.findVirtual(classOf[Foo], "rev", methodType(classOf[String], classOf[String]))
    val mhOverL = l.findVirtual(classOf[Foo], "over", methodType(classOf[String], classOf[Long]))
    val mhOverI = l.findVirtual(classOf[Foo], "over", methodType(classOf[String], classOf[Int]))
    val mhUnit  = l.findVirtual(classOf[Foo], "unit", methodType(classOf[Unit], classOf[String]))
    val mhObj   = l.findVirtual(classOf[Foo], "obj", methodType(classOf[Any], classOf[String]))
    val mhVoid  = l.findVirtual(classOf[Foo], "void", methodType(classOf[String], classOf[Void]))

    assert(-42 == (mhNeg.invokeExact(self, 42): Int))
    assert(-33 == (mhNeg.invokeExact(self, 33): Int))

    assert("oof" == (mhRev.invokeExact(self, "foo"): String))
    assert("rab" == (mhRev.invokeExact(self, "bar"): String))

    assert("long" == (mhOverL.invokeExact(self, 1L): String))
    assert("int" == (mhOverI.invokeExact(self, 1): String))

    assert(-3 == (id(mhNeg.invokeExact(self, 3)): Int))
    expectWrongMethod(mhNeg.invokeExact(self, 4))

    { mhUnit.invokeExact(self, "hi"): Unit; () } // explicit block
    val hi2: Unit = mhUnit.invokeExact(self, "hi2")
    assert((()) == (hi2: Any))
    def hi3: Unit = mhUnit.invokeExact(self, "hi3")
    assert((()) == (hi3: Any))

    { mhObj.invokeExact(self, "any"); () } // explicit block
    val any2 = mhObj.invokeExact(self, "any2")
    assert("any2" == any2)
    def any3 = mhObj.invokeExact(self, "any3")
    assert("any3" == any3)

    assert("void" == (mhVoid.invokeExact(self, null: Void): String))

    expectWrongMethod {
      l // explicit chain method call
        .findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
        .invokeExact(self, 3)
    }
    val res4 = {
      l // explicit chain method call
        .findVirtual(classOf[Foo], "neg", methodType(classOf[Int], classOf[Int]))
        .invokeExact(self, 4): Int
    }
    assert(-4 == res4)
  }

  def id[T](x: T): T = x

  def expectWrongMethod(op: => Any) = try {
    op
    throw new AssertionError("expected operation to fail but it didn't")
  } catch { case expected: WrongMethodTypeException => () }

}
