package scala.collection.immutable

import org.junit.jupiter.api.Assertions

object SeqTests {
  def checkSearch[A](seq: Seq[A], needle: A, ord: Ordering[A]): Unit = {
    import scala.collection.Searching.{Found, InsertionPoint}
    val size = seq.size
    val indices = List(-1, -10, -99, Int.MinValue, 0, size -1, size, size + 1, size + 10, Int.MaxValue)
    for {
      from <- indices
      to <- indices
    } {
      val sorted = seq.sorted(ord)
      sorted.search(needle, from, to)(ord) match {
        case Found(foundIndex: Int) => {
          val found = sorted(foundIndex)
          Assertions.assertTrue(ord.equiv(found, needle), s"found value $found not equivalent to searched value $needle in List(0-1000) between indices $from and $to")
        }
        case InsertionPoint(insertionPoint: Int) =>
          for ((e, i) <- sorted.zipWithIndex) {
            if(i >= from && i < to) {
              if(i < insertionPoint) Assertions.assertFalse(ord.gt(e, needle), s"a value before the insertion point was greater than $needle")
              else Assertions.assertFalse(ord.lt(e, needle), s"a values past the insertion point was smaller than $needle")
            }
            //else we don't want to make any statements for values out of that range
          }
      }
    }
  }
}
