package scala.reflect.internal

import scala.tools.testing.AssertUtil._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert._
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class NamesTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  val h1 = newTermName("hai")
  val h2 = newTermName("hai")
  val f  = newTermName("fisch")

  val h1y = h1.toTypeName
  val h2y = newTypeName("hai")
  val fy  = newTypeName("fisch")

  val uy = newTypeName("uhu")
  val u  = uy.toTermName // calling toTermName after constructing a typeName. This tests the fact
                         // that creating a typeName always also first creates a termName. There is
                         // an assertion for that in toTermName.

  @Test
  def termNamesAreHashConsed() {
    assertTrue(h1 eq h2)
    assertEquals(h1, h2)
    assertTrue(h1 ne f)
    assertTrue(h1 != f)
  }

  @Test
  def termNamesNotEqualsTypeNames() {
    assert(h1 ne h1y)
    assert(h1 != h1y)
    assert(h2 ne h2y)
    assert(h2 != h2y)
  }

  @Test
  def termNamesTypeNamesSameRange() {
    assert(h1.start == h1y.start && h1.length == h1y.length)
    assert(h2.start == h2y.start && h2.length == h2y.length)
    assert(u.start == uy.start && u.length == uy.length)
  }

  @Test
  def testLookupTypeName() {
    assert(lookupTypeName("hai".toCharArray) eq h1y)
    assert(lookupTypeName("fisch".toCharArray) eq fy)
    assert(lookupTypeName("uhu".toCharArray) eq uy)

    assertThrows[AssertionError](lookupTypeName("dog".toCharArray), _ contains "not yet created")
    val d = newTermName("dog")
    assertThrows[AssertionError](lookupTypeName("dog".toCharArray), _ contains "not yet created")
    val dy = d.toTypeName
    assert(lookupTypeName("dog".toCharArray) eq dy)
  }

  @Test
  def emptyName() {
    val z = newTermName("")
    val zy = z.toTypeName
    assertEquals(z.toString, "")
    assertEquals(zy.toString, "")
    assert(z eq newTermName(""))
    assert(zy eq newTypeName(""))
  }

  @Test
  def subNameTest() {
    val i = f.subName(1, f.length)
    assert(i.start == (f.start + 1) && i.length == (f.length - 1))
    assert(f.subName(0, f.length) eq f)

    val iy = fy.subName(1, fy.length)
    assert(iy.start == (fy.start + 1) && iy.length == (fy.length - 1))
    assert(fy.subName(0, fy.length) eq fy)

    assert(f.subName(1,1) eq newTermName(""))
    assert(f.subName(1, 0) eq newTermName(""))

    assertThrows[IllegalArgumentException](f.subName(0 - f.start - 1, 1))
  }

  @Test
  def stringEqualsTest() {
    assert(h1 string_== h2)
    assert(h1 string_== h1y)
  }
}
