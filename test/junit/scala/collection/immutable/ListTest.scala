package scala.collection.immutable

import org.junit.Assert.{assertSame, assertEquals}
import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.ref.WeakReference

@RunWith(classOf[JUnit4])
class ListTest {
  /**
   * Test that empty iterator does not hold reference
   * to complete List
   */
  @Test
  def testIteratorGC(): Unit = {
    var num = 0
    var emptyIterators = Seq.empty[(Iterator[Int], WeakReference[List[Int]])]

    do {
      val list = List.fill(10000)(num)
      val ref = WeakReference(list)

      val i = list.iterator

      while (i.hasNext) i.next()

      emptyIterators = (i, ref) +: emptyIterators

      num+=1
    } while (emptyIterators.forall(_._2.get.isDefined) && num<1000)

    // check something is result to protect from JIT optimizations
    for ((i, _) <- emptyIterators) {
      Assert.assertTrue(i.isEmpty)
    }

    // await gc up to ~5 seconds
    var forceLoops = 50
    while (emptyIterators.forall(_._2.get.isDefined) && forceLoops>0) {
      System.gc()
      Thread.sleep(100)
      forceLoops -= 1
    }

    // real assertion
    Assert.assertTrue(emptyIterators.exists(_._2.get.isEmpty))
  }

  @Test
  def updated(): Unit = {
    val xs = 1 :: 2 :: Nil
    Assert.assertEquals(0 :: 2 :: Nil, xs.updated(index = 0, elem = 0))
    Assert.assertEquals(1 :: 0 :: Nil, xs.updated(index = 1, elem = 0))
    try {
      xs.updated(index = -1, 0)
      Assert.fail("No exception thrown")
    } catch {
      case e: IndexOutOfBoundsException => ()
    }
    try {
      xs.updated(index = 2, 0)
      Assert.fail("No exception thrown")
    } catch {
      case e: IndexOutOfBoundsException => ()
    }
  }

  @Test
  def factoryReuse(): Unit = {
    val ls = List("a")
    assertSame(ls, List.apply(ls: _*))
    assertSame(ls, List.from(ls))
  }

  @Test def checkSearch: Unit = SeqTests.checkSearch(List(0 to 1000: _*), 15,  implicitly[Ordering[Int]])

  @Test def applyOrElse(): Unit = {
    val f: Int => String = "_" + _.toString

    def slowApplyOrElse(list: List[String], i: Int): String = {
      if (list.isDefinedAt(i)) list(i)
      else f(i)
    }

    def check(list: List[String], i: Int): Unit =
      assertEquals(s"i=$i, size=${list.size}", slowApplyOrElse(list, i), list.applyOrElse(i, f))

    for {
      size <- (0 to 10) ++ List(20, 50, 100, 1000)
      list = List.tabulate(size)(_.toString)
      i <- List(-1000, -100, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6,7,8,9,10, 100, 1000, 10000, 100000)
    } check(list, i)
  }
}
