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

      num += 1
    } while (emptyIterators.forall(_._2.get.isDefined) && num < 1000)

    // check something is result to protect from JIT optimizations
    for ((i, _) <- emptyIterators) {
      Assert.assertTrue(i.isEmpty)
    }

    // await gc up to ~5 seconds
    var forceLoops = 50
    while (emptyIterators.forall(_._2.get.isDefined) && forceLoops > 0) {
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

  @Test def checkSearch: Unit =
    SeqTests.checkSearch(List(0 to 1000: _*), 15, implicitly[Ordering[Int]])

  @Test def transpose: Unit = {

    for {
      h <- 0 to 20
      w <- 0 to 20
    } {

      val list: Seq[IndexedSeq[(Int, Int)]] = List.from(
        for {
          x <- 0 until h
        } yield
          for {
            y <- 0 until w
          } yield (x, y)
      )


      val transposed: Seq[IndexedSeq[(Int, Int)]] = List.from(
        if (h * w == 0) Nil
        else for {
          y <- 0 until w
        } yield
          for {
            x <- 0 until h
          } yield (x, y)
      )

      assertEquals(s"h=$h,w=$w,list=\n$list",transposed, list.transpose)
    }

    assertEquals(List(), List().transpose)
    assertEquals(List(List(1)), List(List(1)).transpose)
    assertEquals(List(List(1, 2)), List(List(1), List(2)).transpose)
    assertEquals(
      List(List(1, 2), List(3, 4)),
      List(List(1, 3), List(2, 4)).transpose
    )
  }
}
