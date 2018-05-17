package scala

import org.junit.Test
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class PartialFunctionTest {

  private val odds = new PartialFunction[Int, String] {
    def isDefinedAt(x: Int): Boolean = x % 2 != 0
    def apply(x: Int): String = x.toString
    override def toString: String = "odds"
  }

  private val evens = new PartialFunction[Int, String] {
    def isDefinedAt(x: Int): Boolean = x % 2 == 0
    def apply(x: Int): String = x.toString
    override def toString: String = "evens"
  }

  private val stringToInt = new PartialFunction[String, Int] {
    def isDefinedAt(x: String): Boolean = {
      try {
        x.toInt
        true
      } catch { case _: NumberFormatException =>
        false
      }
    }
    def apply(x: String): Int = x.toInt
    override def toString: String = "stringToInt"
  }

  @Test
  def orElseToString: Unit = {
    val orElse = odds.orElse(evens)
    assertEquals("odds.orElse(evens)", orElse.toString)
  }

  @Test
  def andThenToString: Unit = {
    val andThen = odds.andThen(stringToInt)
    assertEquals("odds.andThen(stringToInt)", andThen.toString)
  }

}
