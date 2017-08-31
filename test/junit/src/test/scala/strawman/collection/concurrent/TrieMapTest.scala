package strawman.collection.concurrent

import strawman.collection.{IterableOnce, View}
import strawman.collection.immutable.{List, Nil}
import org.junit.{Assert, Test}

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
}
