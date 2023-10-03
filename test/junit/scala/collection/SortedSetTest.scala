package scala.collection

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.annotation.nowarn

@RunWith(classOf[JUnit4])
@nowarn("cat=w-flag-value-discard")
class SortedSetTest {
  import SortedSetTest._

  private var count: Int = 0
  private def genOrd: Ordering[Box[Int]] =
    Ordering.by(box => { count += 1; box.value })
  implicit private val ord: Ordering[Box[Int]] = genOrd

  private val sets = {
    val values = (1 to 20).map(Box(_))
    Seq[collection.SortedSet[Box[Int]]](
      values.to(immutable.SortedSet),
      values.to(mutable.SortedSet),
    )
  }

  @Test
  def min(): Unit = {
    for (set <- sets) {
      count = 0
      set.min(ord)
      assert(count == 0)

      count = 0
      set.min(ord.reverse)
      assert(count == 0)

      count = 0
      set.min(genOrd)
      assert(count > 10)
    }
  }

  @Test
  def max(): Unit = {
    for (set <- sets) {
      count = 0
      set.max(ord)
      assert(count == 0)

      count = 0
      set.max(ord.reverse)
      assert(count == 0)

      count = 0
      set.max(genOrd)
      assert(count > 10)
    }
  }

  @Test
  def min_max_differentOrdering(): Unit = {
    implicit val forward: Ordering[Int] = (x, y) => Ordering.Int.compare(x, y)

    val set = SortedSet(1, 2, 3)(Ordering.Int.reverse)
    assertEquals(1, set.min)
    assertEquals(3, set.max)
    assertEquals(3, set.min(set.ordering))
    assertEquals(1, set.max(set.ordering))
  }
}

private object SortedSetTest {
  final case class Box[A](value: A)
}
