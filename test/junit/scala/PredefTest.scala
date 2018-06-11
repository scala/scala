package scala

import org.junit.Assert._
import org.junit.Test


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
  def testAnyTap: Unit = {
    var x: Int = 0
    val result = List(1, 2, 3)
      .tap(xs => x = xs(0))
    assertEquals(1, x)
    assertEquals(List(1, 2, 3), result)
  }

  @Test
  def testAnyPipe: Unit = {
    val result = List(1, 2, 3)
      .pipe(xs => xs.mkString(","))
    assertEquals("1,2,3", result)
  }
}
