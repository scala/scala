package scala.collection.immutable

import org.junit.Assert.{assertEquals, assertSame}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class HashSetTest {

  @Test
  def factoryReuse(): Unit = {
    assertSame(HashSet.empty, HashSet.empty)
    assertSame(HashSet.empty, HashSet())
    val m = HashSet("a")
    assertSame(m, HashSet.from(m))
  }
}
