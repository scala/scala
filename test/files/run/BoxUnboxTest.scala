import org.junit.Assert._
import scala.tools.testkit.AssertUtil._

class VCI(val x: Int) extends AnyVal { override def toString = "" + x }

object Test {

  def main(args: Array[String]): Unit = {
    boxUnboxInt()
    numericConversions()
    boxUnboxBoolean()
    boxUnboxUnit()
    t9671()
  }

  def boxUnboxInt(): Unit = {

    def genericNull[T] = null.asInstanceOf[T] // allowed, see scala/bug#4437, point 2

    val b = Integer.valueOf(1)
    val u = 1

    assertEquals(1.toInt, u)

    assertEquals(Predef.int2Integer(1), b)
    assertEquals(1: Integer, b)
    assertEquals(Int.box(1), b)
    assertEquals(1.asInstanceOf[Object], b)

    assertThrows[ClassCastException]("".asInstanceOf[Integer])

    assertEquals(Predef.Integer2int(b), u)
    assertEquals(b: Int, u)
    assertEquals(Int.unbox(b), u)
    assertEquals(b.asInstanceOf[Int], u)
    assertEquals(b.intValue, u)
    assertEquals(b.toInt, u)
    intWrapper(b).toInt

    assertThrows[ClassCastException](Int.unbox(""))
    assertThrows[ClassCastException]("".asInstanceOf[Int])

    // null unboxing in various positions

    val n1 = Int.unbox(null)
    assertEquals(n1, 0)
    val n2 = Predef.Integer2int(null)
    assertEquals(n2, 0)
    val n3 = (null: Integer): Int
    assertEquals(n3, 0)
    val n4 = null.asInstanceOf[Int]
    assertEquals(n4, 0)
    val n5 = null.asInstanceOf[Int] == 0
    assertTrue(n5)
    val n6 = null.asInstanceOf[Int] == null
    assertFalse(n6)
    val n7 = null.asInstanceOf[Int] != 0
    assertFalse(n7)
    val n8 = null.asInstanceOf[Int] != null
    assertTrue(n8)

    val mp = new java.util.HashMap[Int, Int]
    val n9 = mp.get(0)
    assertEquals(n9, 0)
    val n10 = mp.get(0) == null                    // scala/bug#602
    assertThrows[AssertionError](assertFalse(n10)) // should not throw

    def f(a: Any) = "" + a
    val n11 = f(null.asInstanceOf[Int])
    assertEquals(n11, "0")

    def n12 = genericNull[Int]
    assertEquals(n12, 0)
  }

  def numericConversions(): Unit = {

    val i1 = 1L.asInstanceOf[Int]
    assertEquals(i1, 1)
    assertThrows[ClassCastException] {
      val i2 = (1L: Any).asInstanceOf[Int] // scala/bug#1448, should not throw. see also scala/bug#4437 point 1.
      assertEquals(i2, 1)
    }
  }

  def boxUnboxBoolean(): Unit = {
    val n1 = Option(null.asInstanceOf[Boolean])
    assertEquals(n1, Some(false))
  }

  def boxUnboxUnit(): Unit = {
    // should not use assertEquals in this test: it takes two Object parameters. normally, Unit does
    // not conform to Object, but for Java-defined methods scalac makes an exception and treats them
    // as Any. passing a Unit as Any makes the compiler go through another layer of boxing, so it
    // can hide some bugs (where we actually have a null, but the compiler makes it a ()).

    var v = 0
    def eff() = { v = 1 }
    def chk() = { assert(v == 1); v = 0 }

    val b = runtime.BoxedUnit.UNIT

    def boxing(x: Unit): runtime.BoxedUnit = {
      val k = this.getClass.getClassLoader.loadClass("scala.Unit$")
      val u = k.getDeclaredField("MODULE$").get(null)
      k.getDeclaredMethods.find(_.getName == "box").get.invoke(u, x.asInstanceOf[Object]).asInstanceOf[runtime.BoxedUnit]
    }
    def unboxing(x: Object): Unit = {
      val k = this.getClass.getClassLoader.loadClass("scala.Unit$")
      val u = k.getDeclaredField("MODULE$").get(null)
      k.getDeclaredMethods.find(_.getName == "unbox").get.invoke(u, x).asInstanceOf[Unit]
    }

    assert(eff() == b); chk()
    //assert(Unit.box(eff()) == b); chk()
    assert(boxing(eff()) == b); chk()
    assert(().asInstanceOf[Object] == b)

    //Unit.unbox({eff(); b}); chk()
    //Unit.unbox({eff(); null}); chk()
    //assertThrows[ClassCastException](Unit.unbox({eff(); ""})); chk()
    unboxing({eff(); b}); chk()
    unboxing({eff(); null}); chk()
    assertThrows[ClassCastException](
      try unboxing({eff(); ""}) catch { case t: java.lang.reflect.InvocationTargetException => throw t.getCause }
    ); chk()

    val n1 = null.asInstanceOf[Unit]
    assert(n1 == b)

    val n2 = null.asInstanceOf[Unit] == b
    assert(n2)

    def f(a: Any) = "" + a
    val n3 = f(null.asInstanceOf[Unit])
    assertEquals(n3, "()")
  }

  def t9671(): Unit = {

    def f1(a: Any) = "" + a
    def f2(a: AnyVal) = "" + a
    def f3[T](a: T) = "" + a
    def f4(a: Int) = "" + a
    def f5(a: VCI) = "" + a
    def f6(u: Unit) = "" + u

    def n1: AnyRef = null
    def n2: Null = null
    def n3: Any = null
    def n4[T]: T = null.asInstanceOf[T]

    def npe(s: => String) = try { s; throw new Error() } catch { case _: NullPointerException => "npe" }

    val result =
          f1(null.asInstanceOf[Int])  +
          f1(  n1.asInstanceOf[Int])  +
          f1(  n2.asInstanceOf[Int])  +
          f1(  n3.asInstanceOf[Int])  +
          f1(               n4[Int])  + // "null"
      "-"                             +
          f1(null.asInstanceOf[VCI])  +
      npe(f1(  n1.asInstanceOf[VCI])) + // scala/bug#8097
          f1(  n2.asInstanceOf[VCI])  +
      npe(f1(  n3.asInstanceOf[VCI])) + // scala/bug#8097
          f1(               n4[VCI])  + // "null"
      "-"                             +
          f1(null.asInstanceOf[Unit]) +
          f1(  n1.asInstanceOf[Unit]) +
          f1(  n2.asInstanceOf[Unit]) +
          f1(  n3.asInstanceOf[Unit]) +
          f1(               n4[Unit]) + // "null"
      "-"                             +
          f2(null.asInstanceOf[Int])  +
          f2(  n1.asInstanceOf[Int])  +
          f2(  n2.asInstanceOf[Int])  +
          f2(  n3.asInstanceOf[Int])  +
          f2(               n4[Int])  + // "null"
      "-"                             +
          f2(null.asInstanceOf[VCI])  +
      npe(f2(  n1.asInstanceOf[VCI])) + // scala/bug#8097
          f2(  n2.asInstanceOf[VCI])  +
      npe(f2(  n3.asInstanceOf[VCI])) + // scala/bug#8097
          f2(               n4[VCI])  + // "null"
      "-"                             +
          f2(null.asInstanceOf[Unit]) +
          f2(  n1.asInstanceOf[Unit]) +
          f2(  n2.asInstanceOf[Unit]) +
          f2(  n3.asInstanceOf[Unit]) +
          f2(               n4[Unit]) + // "null"
      "-"                             +
          f3(null.asInstanceOf[Int])  +
          f3(  n1.asInstanceOf[Int])  +
          f3(  n2.asInstanceOf[Int])  +
          f3(  n3.asInstanceOf[Int])  +
          f3(               n4[Int])  + // "null"
      "-"                             +
          f3(null.asInstanceOf[VCI])  +
      npe(f3(  n1.asInstanceOf[VCI])) + // scala/bug#8097
          f3(  n2.asInstanceOf[VCI])  +
      npe(f3(  n3.asInstanceOf[VCI])) + // scala/bug#8097
          f3(               n4[VCI])  + // "null"
      "-"                             +
          f3(null.asInstanceOf[Unit]) +
          f3(  n1.asInstanceOf[Unit]) +
          f3(  n2.asInstanceOf[Unit]) +
          f3(  n3.asInstanceOf[Unit]) +
          f3(               n4[Unit]) + // "null"
      "-"                             +
          f4(null.asInstanceOf[Int])  +
          f4(  n1.asInstanceOf[Int])  +
          f4(  n2.asInstanceOf[Int])  +
          f4(  n3.asInstanceOf[Int])  +
          f4(               n4[Int])  +
      "-"                             +
          f5(null.asInstanceOf[VCI])  +
      npe(f5(  n1.asInstanceOf[VCI])) + // scala/bug#8097
          f5(  n2.asInstanceOf[VCI])  +
      npe(f5(  n3.asInstanceOf[VCI])) + // scala/bug#8097
      npe(f5(               n4[VCI])) + // scala/bug#8097
      "-"                             +
          f6(null.asInstanceOf[Unit]) +
          f6(  n1.asInstanceOf[Unit]) +
          f6(  n2.asInstanceOf[Unit]) +
          f6(  n3.asInstanceOf[Unit]) +
          f6(               n4[Unit])   // "null"
    assertEquals(result,
      "0000null-0npe0npenull-()()()()null-0000null-0npe0npenull-()()()()null-0000null-0npe0npenull-()()()()null-00000-0npe0npenpe-()()()()null")
  }

}
