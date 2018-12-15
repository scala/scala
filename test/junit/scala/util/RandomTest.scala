package scala.util

import org.junit.{ Assert, Test }
import scala.tools.testing.AssertUtil.assertSameElements

class RandomTest {
  // Test for scala/bug#9059
  @Test def testAlphanumeric: Unit = {
    def isAlphaNum(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

    val items = Random.alphanumeric.take(100000)
    for (c <- items) {
      Assert.assertTrue(s"$c should be alphanumeric", isAlphaNum(c))
    }
  }

  @Test def test11316: Unit = {

    val shuffled = Random.shuffle("hello world!".toSeq)

    assertSameElements(shuffled.sorted, "hello world!".toSeq.sorted)

  }
}
