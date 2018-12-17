package scala.util

import org.junit.{Assert, Test}

import scala.collection.immutable.WrappedString

class RandomTest {
  // Test for scala/bug#9059
  @Test def testAlphanumeric: Unit = {
    def isAlphaNum(c: Char) = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

    val items = Random.alphanumeric.take(100000)
    for (c <- items) {
      Assert.assertTrue(s"$c should be alphanumeric", isAlphaNum(c))
    }
  }

  // These tests check that the static type returned by `Random.shuffle` is as specific as possible
  @Test def testShuffle: Unit = {
    val s = Random.shuffle("bar")
    val sT: WrappedString = s
    val ws = Random.shuffle("bar".toSeq)
    val wsT: WrappedString = ws
    val lhm = Random.shuffle(collection.mutable.LinkedHashMap("foo" -> 1, "bar" -> 2))
    val lhmT: collection.mutable.LinkedHashMap[String, Int] = lhm
  }
}
