package strawman.collection.test

import scala.{Int, Some, Unit}
import scala.Predef.String

import org.junit.Test
import org.junit.Assert._
import strawman.collection.concurrent.TrieMap

import scala.util.hashing.Hashing
import scala.math.Equiv

class TrieMapTest {

  @Test
  def customHashingAndEquiv_10481(): Unit = {
    val h = new Hashing[Int] { def hash(i: Int) = i % 4 }
    val e = new Equiv[Int] { def equiv(x: Int, y: Int) = (x % 8) == (y % 8) }
    val xs = new TrieMap[Int, String](h, e)
    xs.put(0, "zero")
    assertEquals(Some("zero"), xs.get(0))
    assertEquals(Some("zero"), xs.get(8)) // 8 and 0 are equivalent keys according to our custom equiv
    xs.put(4, "four") // 4 and 0 have the same hash according to our custom hashing, but they
                      // are different keys (collision)
    assertEquals(Some("zero"), xs.get(8))
    assertEquals(Some("four"), xs.get(4))
  }

}
