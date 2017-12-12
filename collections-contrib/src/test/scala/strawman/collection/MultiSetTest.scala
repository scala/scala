package strawman.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.List
import org.junit.{Assert, Test}

@RunWith(classOf[JUnit4])
class MultiSetTest {

  @Test
  def equality(): Unit = {
    val ms1 = MultiSet("a", "b", "b", "c")
    val ms2 = MultiSet("a", "b", "b", "c")

    Assert.assertEquals(ms2, ms1)
    Assert.assertEquals(ms1, ms2)
    Assert.assertEquals(ms1.##, ms2.##)
  }

  @Test
  def concat(): Unit = {
    Assert.assertEquals(
      MultiSet(1, 1),
      MultiSet(1).concat(MultiSet(1))
    )
    Assert.assertEquals(
      MultiSet("a", "a", "a"),
      MultiSet("a").concatOccurrences(List(("a", 2)))
    )
  }

  @Test
  def map(): Unit = {
    Assert.assertEquals(
      MultiSet("A", "B", "B"),
      MultiSet("a", "b", "b").map(_.toUpperCase)
    )
    Assert.assertEquals(
      MultiSet(1, 1),
      MultiSet("a", "b").map(_ => 1)
    )
    Assert.assertEquals(
      MultiSet("c", "c", "c", "c"),
      MultiSet("a", "b", "b").mapOccurrences { _ => ("c", 2) }
    )
  }

}
