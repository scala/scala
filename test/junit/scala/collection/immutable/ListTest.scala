package scala.collection.immutable

import org.junit.Assert._
import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.ref.WeakReference
import scala.tools.testkit.{AllocationTest, CompileTime}
import scala.collection.Sizes

@RunWith(classOf[JUnit4])
class ListTest extends AllocationTest{
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

  @Test def checkSearch(): Unit = SeqTests.checkSearch(List(0 to 1000: _*), 15,  implicitly[Ordering[Int]])


  @Test def colonColonColon(): Unit = {

    assertEquals(Nil, Nil ::: Nil)
    assertEquals(List(1), List(1) ::: List())
    assertEquals(List(1), List() ::: List(1))
    assertEquals(List(1, 2), List(1, 2) ::: List())
    assertEquals(List(1, 2), List(1) ::: List(2))
    assertEquals(List(1, 2), List() ::: List(1, 2))
  }


  @Test def listFrom(): Unit = {
    for {
      dataSize <- 0 to 1000 by 4
      data = 0 until dataSize
      vector = data.to(Vector)
    } {
      assertEquals(data, List.from(data))
      assertEquals(vector, List.from(vector))
    }
  }

  @Test def emptyNonAllocating(): Unit = {
    nonAllocating(List.empty)
    nonAllocating(List())
  }


  @Test def smallListAllocation(): Unit = {
    if (CompileTime.versionNumberString == "2.13.2") return
    exactAllocates(Sizes.list * 1, "list  size 1")(List("0"))
    exactAllocates(Sizes.list * 2, "list  size 2")(List("0", "1"))
    exactAllocates(Sizes.list * 3, "list  size 3")(List("0", "1", ""))
    exactAllocates(Sizes.list * 4, "list  size 4")(List("0", "1", "2", "3"))
    exactAllocates(Sizes.list * 5, "list  size 5")(List("0", "1", "2", "3", "4"))
    exactAllocates(Sizes.list * 6, "list  size 6")(List("0", "1", "2", "3", "4", "5"))
    exactAllocates(Sizes.list * 7, "list  size 7")(List("0", "1", "2", "3", "4", "5", "6"))
  }

  @Test def largeListAllocation(): Unit = {
    def expected(n: Int) = Sizes.list * n + Sizes.wrappedRefArray(n) + Sizes.wrappedRefArrayIterator
    exactAllocates(expected(10), "list  size 10")(
      List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    exactAllocates(expected(20), "list  size 20")(
      List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  }
}
