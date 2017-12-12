package strawman.collection

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import strawman.collection.immutable.List
import org.junit.{Assert, Test}

@RunWith(classOf[JUnit4])
class MultiMapTest {

  @Test
  def equality(): Unit = {
    val mm1 = MultiMap("a" -> 1, "b" -> 0, "b" -> 1, "c" -> 3)
    val mm2 = MultiMap("a" -> 1, "b" -> 0, "b" -> 1, "c" -> 3)

    Assert.assertEquals(mm2, mm1)
    Assert.assertEquals(mm1, mm2)
    Assert.assertEquals(mm1.##, mm2.##)
  }

  @Test
  def access(): Unit = {
    val mm = MultiMap("a" -> 1, "b" -> 0, "b" -> 1, "c" -> 3)

    Assert.assertEquals(Set(0, 1), mm.get("b"))
    Assert.assertEquals(Set.empty, mm.get("d"))
    Assert.assertTrue(mm.containsKey("a"))
    Assert.assertFalse(mm.containsKey("d"))
    Assert.assertTrue(mm.containsEntry("a" -> 1))
    Assert.assertFalse(mm.containsEntry("a" -> 2))
    Assert.assertFalse(mm.containsEntry("d" -> 2))
    Assert.assertTrue(mm.containsValue(1))
    Assert.assertTrue(mm.containsValue(3))
    Assert.assertFalse(mm.containsValue(4))
  }

  @Test
  def concat(): Unit = {
    Assert.assertEquals(
      MultiMap(1 -> true, 1 -> false),
      MultiMap(1 -> true).concat(MultiMap(1 -> false))
    )
    Assert.assertEquals(
      MultiMap("a" -> 1, "a" -> 2, "a" -> 3),
      MultiMap("a" -> 1).concatSets(List("a" -> Set(2, 3)))
    )
  }

  @Test
  def map(): Unit = {
    Assert.assertEquals(
      MultiMap("A" -> 1, "B" -> 1, "B" -> 2),
      MultiMap("a" -> 1, "b" -> 1, "b" -> 2).map { case (k, v) => (k.toUpperCase, v) }
    )
    Assert.assertEquals(
      MultiMap(1 -> true, 1 -> true),
      MultiMap("a" -> true, "b" -> true).map { case (_, v) => 1 -> v }
    )
    Assert.assertEquals(
      MultiMap("c" -> 1, "c" -> 2, "c" -> 3, "c" -> 4),
      MultiMap("a" -> 1, "b" -> 2, "b" -> 3).mapSets { _ => "c" -> Set(1, 2, 3, 4) }
    )
  }

  @Test
  def withFilter(): Unit = {
    val filtered =
      for {
        (k, v) <- MultiMap("a" -> 1, "b" -> 2)
        if k == "a" && v % 2 == 0
      } yield (k, v)
    val filteredT: MultiMap[String, Int] = filtered
    Assert.assertEquals(Seq.empty, filtered.toSeq)
  }

}
