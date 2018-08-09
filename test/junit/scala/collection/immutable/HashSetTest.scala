package scala.collection.immutable

import org.junit.Assert.assertSame
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class HashSetTest {

  @Test
  def factoryReuse(): Unit = {
    assertSame(ChampHashSet.empty, ChampHashSet.empty)
    assertSame(ChampHashSet.empty, ChampHashSet())
    val m = ChampHashSet("a")
    assertSame(m, ChampHashSet.from(m))
  }
}
