package scala.lang.stringinterpol

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.language.implicitConversions

@RunWith(classOf[JUnit4])
class SymbolsTest {

  @Test
  def symbolTest: Unit = {
    assertEquals("'foo", sym"foo".toString)
  }

  @Test
  def emptySymbolTest: Unit = {
    assertEquals("'", sym"".toString)
  }

  @Test
  def symbolListTest: Unit = {
    assertEquals("List('foo, 'bar, 'baz)", List(sym"foo", sym"bar", sym"baz").toString)
    assertEquals("List('foo, 'bar, 'baz)", (sym"foo" :: sym"bar" :: sym"baz" :: Nil).toString)
  }

  @Test
  def symbolInterpolationTest: Unit = {
    val foo = "bar"
    assertEquals("'bar", sym"$foo".toString)
  }

  @Test
  def patternMatchTest: Unit = {
    val foo = "foo"
    sym"foo" match {
      case sym""           => fail
      case sym"$$foo"      => fail
      case sym"$${foo}"    => fail
      case sym"foo$${bar}" => fail
      case sym"foo\n"      => fail
      case sym"foo"        => return
      case _               => fail
    }
  }

  @Test(expected = classOf[IllegalArgumentException])
  def patternMatchDollarFail: Unit = {
    val bar = "foo"
    sym"foo" match {
      // Variable interpolation not supported, use '$$' for '$'
      case sym"$bar" =>
    }
  }

  @Test(expected = classOf[IllegalArgumentException])
  def patternMatchDollarBracesFail: Unit = {
    val bar = "foo"
    sym"foo" match {
      // Variable interpolation not supported, use '$$' for '$'
      case sym"${bar}" =>
    }
  }

  @Test
  def deconstructTest: Unit = {
    val sym"foo" = sym"foo"
  }

  @Test(expected = classOf[MatchError])
  def deconstructFail: Unit = {
    val sym"bar" = sym"foo"
  }

  @Test(expected = classOf[IllegalArgumentException])
  def deconstructDollarFail: Unit = {
    // Variable interpolation not supported, use '$$' for '$'
    val sym"$foo" = sym"foo"
  }
}
