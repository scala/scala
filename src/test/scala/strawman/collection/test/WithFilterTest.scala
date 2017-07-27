package strawman
package collection
package test

import immutable.{List, TreeMap, TreeSet}

import scala.{Char, Int, Unit}
import scala.Predef.{ArrowAssoc, assert}
import org.junit.Test

class WithFilterTest {

  @Test
  def iterables(): Unit = {
    val xs1 =
      for {
        x <- List(1, 2, 3)
        if x % 2 == 0
      } yield x + 1
    val xs1t: List[Int] = xs1
    assert(xs1 == List(3))
  }

  @Test
  def maps(): Unit = {
    val xs1 =
      for {
        (k, v) <- Map(1 -> 'a', 2 -> 'b', 3 -> 'c')
        if k % 2 == 0
      } yield (v, k)
    val xs1t: Map[Char, Int] = xs1
    assert(xs1 == Map('b' -> 2))

    val xs2 =
      for {
        (k, v) <- Map(1 -> 'a', 2 -> 'b', 3 -> 'c')
        if k % 2 == 0
      } yield v
    val xs2t: Iterable[Char] = xs2
    assert(List('b') == xs2)
  }

  @Test
  def sorted(): Unit = {
    val xs1 =
      for {
        x <- TreeSet(1, 2, 3)
        if x % 2 == 0
      } yield x + 1
    val xs1t: TreeSet[Int] = xs1
    assert(xs1 == TreeSet(3))
  }

  @Test
  def sortedMap(): Unit = {
    val xs1 =
      for {
        (k, v) <- TreeMap(1 -> 'a', 2 -> 'b', 3 -> 'c')
        if k % 2 == 0
      } yield (v, k)
    val xs1t: TreeMap[Char, Int] = xs1
    assert(xs1 == TreeMap('b' -> 2))
  }

}
