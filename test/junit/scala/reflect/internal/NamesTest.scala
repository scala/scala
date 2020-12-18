package scala.reflect.internal

import scala.tools.testkit.AssertUtil._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import org.junit.Assert.{ assertThrows => _, _ }
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class NamesTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  val h1 = TermName("hai")
  val h2 = TermName("hai")
  val f  = TermName("fisch")

  val h1y = h1.toTypeName
  val h2y = TypeName("hai")
  val fy  = TypeName("fisch")

  val uy = TypeName("uhu")
  val u  = uy.toTermName // calling toTermName after constructing a typeName. This tests the fact
                         // that creating a typeName always also first creates a termName. There is
                         // an assertion for that in toTermName.

  @Test
  def termNamesAreHashConsed(): Unit = {
    assertTrue(h1 eq h2)
    assertEquals(h1, h2)
    assertTrue(h1 ne f)
    assertTrue(h1 != f)
  }

  @Test
  def termNamesNotEqualsTypeNames(): Unit = {
    assertTrue((h1: AnyRef) ne h1y)
    assertTrue((h1: AnyRef) != h1y)
    assertTrue((h2: AnyRef) ne h2y)
    assertTrue((h2: AnyRef) != h2y)
  }

  @Test
  def termNamesTypeNamesSameRange(): Unit = {
    assertTrue(h1.start == h1y.start && h1.length == h1y.length)
    assertTrue(h2.start == h2y.start && h2.length == h2y.length)
    assertTrue(u.start == uy.start && u.length == uy.length)
  }

  @Test
  def testLookupTypeName(): Unit = {
    assertTrue(lookupTypeName("hai".toCharArray) eq h1y)
    assertTrue(lookupTypeName("fisch".toCharArray) eq fy)
    assertTrue(lookupTypeName("uhu".toCharArray) eq uy)

    assertThrows[AssertionError](lookupTypeName("dog".toCharArray), _ contains "not yet created")
    val d = TermName("dog")
    assertThrows[AssertionError](lookupTypeName("dog".toCharArray), _ contains "not yet created")
    val dy = d.toTypeName
    assertTrue(lookupTypeName("dog".toCharArray) eq dy)
  }

  @Test
  def emptyName(): Unit = {
    val z = TermName("")
    val zy = z.toTypeName
    assertEquals(z.toString, "")
    assertEquals(zy.toString, "")
    assertTrue(z eq TermName(""))
    assertTrue(zy eq TypeName(""))
  }

  @Test
  def subNameTest(): Unit = {
    val i = f.subName(1, f.length)
    assertTrue(i.start == (f.start + 1) && i.length == (f.length - 1))
    assertTrue(f.subName(0, f.length) eq f)

    val iy = fy.subName(1, fy.length)
    assertTrue(iy.start == (fy.start + 1) && iy.length == (fy.length - 1))
    assertTrue(fy.subName(0, fy.length) eq fy)

    assertTrue(f.subName(1,1) eq TermName(""))
    assertTrue(f.subName(1, 0) eq TermName(""))

    assertThrows[IllegalArgumentException](f.subName(0 - f.start - 1, 1))
  }

  @Test
  def stringEqualsTest(): Unit = {
    assertTrue(h1 string_== h2)
    assertTrue(h1 string_== h1y)
  }

  @Test
  def pos(): Unit = {
    def check(nameString: String, sub: String) = {
      val name = TermName(nameString)
      val javaResult = name.toString.indexOf(sub) match { case -1 => name.length case x => x }
      val nameResult = name.pos(sub)
      assertEquals(javaResult, nameResult)
      if (sub.length == 1) {
        val nameResultChar = name.pos(sub.head)
        assertEquals(javaResult, nameResultChar)
      }
    }

    check("a", "a") // was "String index out of range: 1
    check("a", "b")
    check("a", "ab")
    check("a", "ba")
    check("ab", "a")
    check("ab", "b")
    check("ab", "ab")
    check("ab", "ba")
    check("", "x")
    check("", "xy")
  }

  @Test
  def `name.lastIndexOf is name.toString.lastIndexOf`(): Unit = {
    def check(name: Name, sub: String): Unit = {
      val javaResult = name.toString.lastIndexOf(sub)
      val nameResult = name.lastIndexOf(sub)
      assertEquals(s""""$name".lastIndexOf("$sub")""", javaResult, nameResult)
    }
    val d = TermName("x$default$42")
    val e = TermName("")
    check(f,  "isch")  // the isch in fisch
    check(fy, "isch")
    check(f,  "")
    check(e,  "")
    check(e,  "abc")
    check(f,  "ischbiisch")
    check(fy, "ischbiisch")
    check(fy, "fyou")
    check(f,  "disch")
    check(fy, "disch")
    check(f,  "hai")   // happens to be earlier in name array
    check(d,  "$$")    // likely to be earlier in name array
  }
}
