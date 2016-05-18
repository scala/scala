package scala

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil._

@RunWith(classOf[JUnit4])
class BoxUnboxTest {
  def genericNull[T] = null.asInstanceOf[T] // allowed, see SI-4437, point 2

  @Test
  def boxUnboxInt(): Unit = {
    val b = new Integer(1)
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
    val n10 = mp.get(0) == null                    // SI-602
    assertThrows[AssertionError](assertFalse(n10)) // should not throw

    def f(a: Any) = "" + a
    val n11 = f(null.asInstanceOf[Int])
    assertEquals(n11, "0")

    def n12 = genericNull[Int]
    assertEquals(n12, 0)
  }

  @Test
  def numericConversions(): Unit = {
    val i1 = 1L.asInstanceOf[Int]
    assertEquals(i1, 1)
    assertThrows[ClassCastException] {
      val i2 = (1L: Any).asInstanceOf[Int] // SI-1448, should not throw. see also SI-4437 point 1.
      assertEquals(i2, 1)
    }
  }

  @Test
  def boxUnboxBoolean(): Unit = {
    val n1 = Option(null.asInstanceOf[Boolean])
    assertEquals(n1, Some(false))
  }

  @Test
  def boxUnboxUnit(): Unit = {
    // should not use assertEquals in this test: it takes two Object parameters. normally, Unit does
    // not conform to Object, but for Java-defined methods scalac makes an exception and treats them
    // as Any. passing a Unit as Any makes the compiler go through another layer of boxing, so it
    // can hide some bugs (where we actually have a null, but the compiler makes it a ()).

    var v = 0
    def eff() = { v = 1 }
    def chk() = { assert(v == 1); v = 0 }

    val b = runtime.BoxedUnit.UNIT

    assert(eff() == b); chk()
    assert(Unit.box(eff()) == b); chk()
    assert(().asInstanceOf[Object] == b)

    Unit.unbox({eff(); b}); chk()
    Unit.unbox({eff(); null}); chk()
    assertThrows[ClassCastException](Unit.unbox({eff(); ""})); chk()

    val n1 = null.asInstanceOf[Unit]
    assert(n1 == b)

    val n2 = null.asInstanceOf[Unit] == b
    assert(n2)

    def f(a: Any) = "" + a
    val n3 = f(null.asInstanceOf[Unit])
    assertEquals(n3, "()")
  }
}
