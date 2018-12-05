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
  def symbolInterpolation: Unit = {
    val foo = "bar"
    assertEquals("'bar", sym"$foo".toString)
  }

  @Test
  def symbolPatternMatch: Unit = {
    val bar = "foo"
    sym"foo" match {
      case sym""              => fail
      case sym"$bar"          => fail
      case sym"$foo"          => fail
      case sym"${foo}"        => fail
      case sym"bar"           => fail
      case sym"foo$bar"       => fail
      case sym"foo${bar}baz"  => fail
      case sym"${foo}bar$baz" => fail
      case sym"foo"           => return
      case _                  => fail
    }
  }

  @Test
  def symbolDeconstruct: Unit = {
    val sym"foo" = sym"foo"
    val Symbol(foo) = sym"foo"
    assertEquals("foo", foo)
  }

  @Test(expected = classOf[MatchError])
  def symbolMatchFailure: Unit = {
    val sym"bar" = sym"foo"
  }
}
