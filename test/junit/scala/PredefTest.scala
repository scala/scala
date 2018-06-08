package scala

import org.hamcrest.CoreMatchers._
import org.hamcrest.MatcherAssert.assertThat
import org.junit.Assert._
import org.junit.Test

import scala.util.{Failure, Success, Try}


class PredefTest {
  @Test
  def testTuple2Alias: Unit = {
    val tup = "foobar" -> 3
    val char = tup match {
      case str -> i => str.charAt(i)
    }
    assertEquals('b', char)

    val a -> b -> c = false -> 42 -> "bazzz"
    val res: Int = if (a) b else c.length
    assertEquals(false, a)
    assertEquals(42, b)
    assertEquals("bazzz", c)
    assertEquals(5, res)
  }

  @Test
  def given_requirementIsMet_when_requiring_then_selfIsReturned(): Unit = {
    val func = () => "val"

    assertThat(func().requiring(true), equalTo("val"))
    assertThat(func().requiring(true), equalTo("val"))
    assertThat(func().requiring(true, "testRequiring"), equalTo("val"))
    assertThat(func().requiring(_ => true), equalTo("val"))
    assertThat(func().requiring(_ => true, "testRequiring"), equalTo("val"))
  }

  @Test
  def given_requirementIsNotMet_when_requiring_then_IllegalArgumentExceptionIsThrown(): Unit = {
    val func = () => "val"

    assertThrows(classOf[IllegalArgumentException], () => func().requiring(false))
    assertThrows(classOf[IllegalArgumentException], () => func().requiring(false, "testRequiring"), Some("testRequiring"))
    assertThrows(classOf[IllegalArgumentException], () => func().requiring(_ => false))
    assertThrows(classOf[IllegalArgumentException], () => func().requiring(_ => false, "testRequiring"), Some("testRequiring"))
  }

  private def assertThrows[T <: Throwable](exceptionType: Class[T], f: () => Any, message: Option[String] = None): Unit =
    Try(f()) match {
      case Success(_) => fail(s"Expected exception of type $exceptionType")
      case Failure(e) if exceptionType.isInstance(e) =>
        if (message.isDefined) {
          assertThat(e.getMessage, containsString(message.get))
        }
      case Failure(e) => fail(s"Expected exception of type $exceptionType, but was ${e.getClass}")
    }
}