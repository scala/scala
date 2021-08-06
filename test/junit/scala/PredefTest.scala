package scala

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.Test


class PredefTest {
  @Test
  def testTuple2Alias(): Unit = {
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
}
