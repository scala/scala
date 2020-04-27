package scala.collection.immutable

import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import Assert._

import scala.collection.{GenTraversableOnce, mutable}
import scala.collection.generic.CanBuildFrom
import scala.collection.Sizes
import scala.ref.WeakReference
import scala.tools.testing.AllocationTest

@RunWith(classOf[JUnit4])
class ListTest extends AllocationTest {
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

  @Test def emptyNonAllocating(): Unit = {
    nonAllocating(List.empty)
    nonAllocating(List())
  }
  @Test def mapOverListWithListCBF(): Unit = {
    val t1 = List[String]()
    object fn extends Function1[String, String] {
      override def apply(v1: String): String = ""
    }
    assertSame(Nil, nonAllocating(t1 map fn))
    val t2 = List[String]("abc")
    //each :: is 20 bytes (padded to 24)
    exactAllocates(24)(t2 map fn)
  }
  @Test def flatMapOverListWithListCBF(): Unit = {
    val t1 = List[String]()
    object fn1 extends Function1[String, List[String]] {
      override def apply(v1: String): List[String] = Nil
    }
    object fn2 extends Function1[String, GenTraversableOnce[String]] {
      val r = List("xx")
      override def apply(v1: String): GenTraversableOnce[String] = r
    }
    object fn3 extends Function1[String, GenTraversableOnce[String]] {
      val r = List("xx")
      override def apply(v1: String): GenTraversableOnce[String] = {
        if (v1 == "def") Nil
        else r
      }
    }
    assertSame(Nil, nonAllocating(t1 flatMap fn1))
    val t2 = List[String]("abc")
    val t3 = List[String]("abc", "def")
    //flatMap on not empty generates an appender (20 bytes + padding) = 24
    //each :: is 20 bytes (padded to 24)

    //nonAllocating as fn results are Nil
    assertSame(Nil, nonAllocating(t2 flatMap fn1))
    assertSame(Nil, nonAllocating(t3 flatMap fn1))

    //doesnt generate the appender, shares the last (only) list
    assertEquals(List("xx"), nonAllocating(t2 flatMap fn2))

    //generates the appender, one ::, shares the last list
    exactAllocates(48)(t3 flatMap fn2)

    //doesnt generate the appender, shares the last (non empty) list
    assertEquals(List("xx"), nonAllocating(t3 flatMap fn3))
  }

  @Test def collectOverListWithListCBF(): Unit = {
    val t1 = List[String]()
    object fn extends PartialFunction[String, List[String]] {
      override def isDefinedAt(x: String): Boolean = true
      override def apply(v1: String): List[String] = Nil
    }
    assertSame(Nil, nonAllocating(t1 collect fn))
    val t2 = List[String]("abc")
    //each :: is 20 bytes (padded to 24)
    exactAllocates(24)(t2 collect fn)
  }

  @Test def testIsLikeListReusableCBF() {
    //isLikeListReusableCBF assumes that
    // - List.ReusableCBF
    // - collection.immutable.Seq.ReusableCBF
    // - collection.Seq.ReusableCBF
    // - collection.immutable.LinearSeq.ReusableCBF
    // - collection.LinearSeq.ReusableCBF
    // are distinct all generate List instances ( and that other don't behave the same way)
    // If this changes then (at least) the method List#isLikeListReusableCBF should be changed accordingly

    val allCompatibleCBFs = List[CanBuildFrom[_,_,_]](List.ReusableCBF,
      LinearSeq.ReusableCBF, collection.LinearSeq.ReusableCBF,
      Seq.ReusableCBF, collection.Seq.ReusableCBF)

    //really only concerned with separate JVM instance
    assertSame(allCompatibleCBFs.length, allCompatibleCBFs.distinct.length)

    allCompatibleCBFs foreach { cbf =>
      assertSame(Nil, build(0, cbf))
    }

    def build(size: Int, cbf: CanBuildFrom[_, _, _]) = {
      val builder = cbf.apply().asInstanceOf[mutable.Builder[String, _]]
      for (i <- 1 to size) {
        builder += ""
      }
      builder.result()
    }

    val listClass = classOf[::[String]]
    assertEquals(listClass, build(1, List.ReusableCBF).getClass())
    assertEquals(listClass, build(1, collection.immutable.Seq.ReusableCBF).getClass())
    assertEquals(listClass, build(1, collection.Seq.ReusableCBF).getClass())

    assertEquals(listClass, build(2, List.ReusableCBF).getClass())
    assertEquals(listClass, build(2, collection.immutable.Seq.ReusableCBF).getClass())
    assertEquals(listClass, build(2, collection.Seq.ReusableCBF).getClass())


  }

  @Test def smallListAllocation: Unit = {
    exactAllocates(Sizes.list * 1, "list  size 1")(List("0"))
    exactAllocates(Sizes.list * 2, "list  size 2")(List("0", "1"))
    exactAllocates(Sizes.list * 3, "list  size 3")(List("0", "1", ""))
    exactAllocates(Sizes.list * 4, "list  size 4")(List("0", "1", "2", "3"))
    exactAllocates(Sizes.list * 5, "list  size 5")(List("0", "1", "2", "3", "4"))
    exactAllocates(Sizes.list * 6, "list  size 6")(List("0", "1", "2", "3", "4", "5"))
    exactAllocates(Sizes.list * 7, "list  size 7")(List("0", "1", "2", "3", "4", "5", "6"))
  }
  @Test def largeListAllocation: Unit = {
    exactAllocates(Sizes.list * 10 + Sizes.wrappedRefArray(10), "list  size 10")(
      List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"))
    exactAllocates(Sizes.list * 20 + Sizes.wrappedRefArray(20), "list  size 20")(
      List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"))
  }
}
