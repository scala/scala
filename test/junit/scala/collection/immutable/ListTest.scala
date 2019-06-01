package scala.collection.immutable

import org.junit.Assert.assertSame
import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.ref.WeakReference

@RunWith(classOf[JUnit4])
class ListTest {
  @Test
  def foo2: Unit = {

    println(List(List(1,2), List(3, 4), List(5, 6))
        .view
        .tapEach(println)
        .transpose
        .to(List)
      )

  }

  @Test
  def foo: Unit = {
    println("HI")
    val x = LazyList.from(Iterator.from(1)).view
      .tapEach(println)
      .sorted
      .reverse

    val y = x .reverse

    val a =
      (1 to 10)
        .iterator
        .to(LazyList)
      LazyList.from(Iterator.from(1 to 10).tapEach(println)).view.reverse.sorted.take(3).iterator


    println("Done!")

//      val z = y
//      .toList
//    val sv =
//      (1 to 10)
//      .to(LazyList)
//      .view
//      .tapEach(println)
//      .reverse
  }
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
}
