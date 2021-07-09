package scala.collection.immutable

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class ListSetTest {

  @Test
  def t7445(): Unit = {
    val s = ListSet(1, 2, 3, 4, 5)
    assertEquals(ListSet(2, 3, 4, 5), s.tail)
  }

  @Test
  def hasCorrectBuilder(): Unit = {
    val m = ListSet("a", "b", "c", "b", "d")
    assertEquals(List("a", "b", "c", "d"), m.toList)
  }

  @Test
  def hasTailRecursiveDelete(): Unit = {
    val s = ListSet(1 to 50000: _*)
    try s - 25000 catch {
      case e: StackOverflowError => fail("A stack overflow occurred")
    }
  }

  @Test
  def hasCorrectHeadTailLastInit(): Unit = {
    val m = ListSet(1, 2, 3)
    assertEquals(1, m.head)
    assertEquals(ListSet(2, 3), m.tail)
    assertEquals(3, m.last)
    assertEquals(ListSet(1, 2), m.init)
  }

  @Test
  def hasCorrectAddRemove(): Unit = {
    val m = ListSet(1, 2, 3)
    assertEquals(ListSet(1, 2, 3, 4), m + 4)
    assertEquals(ListSet(1, 2, 3), m + 2)
    assertEquals(ListSet(2, 3), m - 1)
    assertEquals(ListSet(1, 3), m - 2)
    assertEquals(ListSet(1, 2, 3), m - 4)
  }

  @Test
  def hasCorrectIterator(): Unit = {
    val s = ListSet(1, 2, 3, 5, 4)
    assertEquals(List(1, 2, 3, 5, 4), s.iterator.toList)
  }

  @Test
  def hasCorrectOrderAfterPlusPlus(): Unit = {
    val foo = ListSet(1)
    var bar = foo ++ ListSet()
    assertEquals(List(1), bar.iterator.toList)

    bar = foo ++ ListSet(1)
    assertEquals(List(1), bar.iterator.toList)

    bar = foo ++ ListSet(2)
    assertEquals(List(1, 2), bar.iterator.toList)

    bar = foo ++ ListSet(1, 2)
    assertEquals(List(1, 2), bar.iterator.toList)

    bar = foo ++ ListSet(1, 2, 3)
    assertEquals(List(1, 2, 3), bar.iterator.toList)

    bar = foo ++ ListSet(1, 2, 3, 4)
    assertEquals(List(1, 2, 3, 4), bar.iterator.toList)

    bar = foo ++ ListSet(1, 2, 3, 4, 5)
    assertEquals(List(1, 2, 3, 4, 5), bar.iterator.toList)

    bar = foo ++ ListSet(1, 2, 3, 4, 5, 6)
    assertEquals(List(1, 2, 3, 4, 5, 6), bar.iterator.toList)
  }

  @Test
  def smallPlusPlus1(): Unit = {
    def check(l1: ListSet[Int], l2: ListSet[Int]) = {
      val expected = l1.iterator.toList ++ l2.iterator.filterNot(l1).toList
      val actual = (l1 ++ l2).iterator.toList
      assertEquals(expected, actual)
    }

    for (start0 <- 0 until 6;
         end0 <- start0 until 6;
         start1 <- 0 until 6;
         end1 <- start1 until 6) {
      val ls0 = ListSet((start0 until end0): _*)
      val ls1 = ListSet((start1 until end1): _*)
      check(ls0, ls1)
    }
  }
  @Test
  def smallPlusPlusAfter(): Unit = {
    def check(l1: ListSet[Int], l2: ListSet[Int]) = {
      val expected = l1.iterator.toList ++ l2.iterator.filterNot(l1).toList
      val actual = (l1 ++ l2).iterator.toList
      assertEquals(expected, actual)
    }

    for (start0 <- 0 until 9;
         end0 <- start0 until 9;
         start1 <- 10 until 19;
         end1 <- start1 until 19) {
      val ls0 = ListSet((start0 until end0): _*)
      val ls1 = ListSet((start1 until end1): _*)
      check(ls0, ls1)
    }
  }
}
