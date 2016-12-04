package scala.util

import org.junit.{ Assert, Test }

class RandomTest {
  // Test for SI-9059
  @Test def testAlphanumeric: Unit = {
    def isAlphaNum(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

    val items = Random.alphanumeric.take(100000)
    for (c <- items) {
      Assert.assertTrue(s"$c should be alphanumeric", isAlphaNum(c))
    }
  }

  @Test def testNextLongN: Unit = {
    val testsPerN = 1000
    val nCats = 100
    def testOneN(n:Long): Unit = {
      for (_ <- 0.until(testsPerN)) {
        val rN = Random.nextLong(n)
        val inRange = (0 <= rN) && (rN < n)
        Assert.assertTrue(s"$rN should be within [0, $n)", inRange)
      }
    }

    for (_ <- 0.until(nCats)) testOneN(Math.abs(Random.nextLong()))
  }
}
