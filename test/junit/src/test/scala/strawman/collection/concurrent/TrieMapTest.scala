package strawman.collection.concurrent

import strawman.collection.{IterableOnce, View}
import strawman.collection.immutable.{List, Nil}
import org.junit.{Assert, Test}

import scala.util.hashing.Hashing

class TrieMapTest {

  private def check[T](result2: List[Any])(f: TrieMap[String, String] => IterableOnce[Any]) = {
    val m = TrieMap[String, String]()
    val values = f(m)
    m.put("k", "v")
    Assert.assertEquals(Nil, List.fromIterable(View.fromIterator(values.iterator())))
    Assert.assertEquals(result2, List.fromIterable(View.fromIterator(f(m).iterator())))
  }

  @Test
  def iterator(): Unit = {
    check(List(("k", "v")))(_.iterator())
  }

  @Test
  def values(): Unit = {
    check(List("v"))(_.values)
  }

  @Test
  def valuesIterator(): Unit = {
    check(List("v"))(_.valuesIterator())
  }

  @Test
  def keySet(): Unit = {
    check(List("k"))(_.keySet)
  }

  @Test
  def keysIterator(): Unit = {
    check(List("k"))(_.keysIterator())
  }

  @Test
  def keys(): Unit = {
    check(List("k"))(_.keys)
  }

  @Test
  def filterKeys(): Unit = {
    check(List(("k", "v")))(_.filterKeys(_ => true))
  }

  @Test
  def mapValues(): Unit = {
    check(List(("k", "v")))(_.mapValues(x => x))
  }

  @Test
  def customHashingAndEquiv_10481(): Unit = {
    val h = new Hashing[Int] { def hash(i: Int) = i % 4 }
    val e = new Equiv[Int] { def equiv(x: Int, y: Int) = (x % 8) == (y % 8) }
    val xs = new TrieMap[Int, String](h, e)
    xs.put(0, "zero")
    Assert.assertEquals(Some("zero"), xs.get(0))
    Assert.assertEquals(Some("zero"), xs.get(8)) // 8 and 0 are equivalent keys according to our custom equiv
    xs.put(4, "four") // 4 and 0 have the same hash according to our custom hashing, but they
    // are different keys (collision)
    Assert.assertEquals(Some("zero"), xs.get(8))
    Assert.assertEquals(Some("four"), xs.get(4))
  }
}
