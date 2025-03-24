package scala.collection
package immutable

import org.junit.Assert._
import org.junit.Test

import java.io.NotSerializableException
import scala.annotation.unused
import scala.collection.immutable.LazyListTest.sd
import scala.collection.mutable.{Builder, ListBuffer}
import scala.tools.testkit.AssertUtil
import scala.util.Try

class LazyListTest {

  /*
  def countAlloc[T](f: => T): Int = {
    locally(LazyList.empty) // init
    LazyList.k = 0
    f
    LazyList.k
  }

  @Test def counts(): Unit = {
    // N*3
    println(countAlloc((1 #:: 2 #:: 3 #:: 4 #:: LazyList.empty).toList))

    // N*4
    println(countAlloc(LazyList.from(1).take(10).toList))

    // N*6
    println(countAlloc(LazyList.from(1).take(20).take(10).toList))
  }
  */

  @Test def consNull(): Unit = {
    val ll = LazyList.cons(1, LazyList.cons(2, null))
    assert(ll.head == 1)
    assert(ll.tail.head == 2)
    locally(ll.tail.tail)
    AssertUtil.assertThrows[NullPointerException](ll.tail.tail.head)

    val ll1 = LazyList.cons[AnyRef](null, null)
    assert(ll1.head == null)
    locally(ll1.tail)
    AssertUtil.assertThrows[NullPointerException](ll1.tail.head)
  }

  @Test def throwEval(): Unit = {
    var bad = true
    val ll = 1 #:: { if (bad) { bad = false; throw new RuntimeException() }; 2} #:: LazyList.empty
    try ll.toList catch { case _: RuntimeException => () }
    assertTrue(ll.toList == List(1, 2))
  }

  @Test def racySerialization(): Unit = {
    import sd._
    val ll = 1 #:: { Thread.sleep(500); 2} #:: LazyList.empty
    new Thread(() => println(ll.toList)).start()
    Thread.sleep(200)
    AssertUtil.assertThrows[NotSerializableException](serialize(ll), _.contains("MidEvaluation"))
  }

  @Test def storeNull(): Unit = {
    val l = "1" #:: null #:: "2" #:: LazyList.empty
    assert(l.toList == List("1", null, "2"))
    assert(l.tail.head == null)
    assert(!l.tail.isEmpty)
  }

  @Test def nse(): Unit = {
    val l = 1 #:: 2 #:: LazyList.empty
    AssertUtil.assertThrows[NoSuchElementException](l.tail.tail.head)
    AssertUtil.assertThrows[UnsupportedOperationException](l.tail.tail.tail)
  }

  @Test
  def serialization(): Unit = {
    import sd._

    val emptyD = serializeDeserialize(LazyList.empty)
    assertNotSame(LazyList.empty, emptyD) // deserialization creates a new instance with both fields `null`
    assertEquals(LazyList.empty, emptyD)

    val l = LazyList.from(10)

    val ld1 = serializeDeserialize(l)
    assertEquals(l.take(10).toList, ld1.take(10).toList)

    l.tail.head
    val ld2 = serializeDeserialize(l)
    assertEquals(l.take(10).toList, ld2.take(10).toList)

    // this used to be a test for scala/scala#10118
    // we used to have: `knownIsEmpty = stateEvaluated && (state eq State.Empty)` a forged stream could have
    //   `stateEvaluated = true` but a non-evaluated `state` lazy val, leading to lazyState evaluation.
    // after scala/scala#10942, the test no longer makes sense, as the original attack path is no longer possible.
    //   we have `knownIsEmpty = stateDefined && isEmpty`, if `!stateDefined` then `isEmpty` can no longer trigger
    //   `lazyState` evaluation
    LazyListTest.serializationForceCount = 0
    val u = LazyList.from(10).map(x => { LazyListTest.serializationForceCount += 1; x })

    assertEquals(LazyListTest.serializationForceCount, 0)

    u.head
    assertEquals(LazyListTest.serializationForceCount, 1)

    val data = serialize(u)

    val ud = deserialize(data).asInstanceOf[LazyList[Int]]

    assertEquals(LazyListTest.serializationForceCount, 1)

    ud.tail.head
    assertEquals(LazyListTest.serializationForceCount, 2)

    u.tail.head
    assertEquals(LazyListTest.serializationForceCount, 3)
  }

  @Test
  def t6727_and_t6440_and_8627(): Unit = {
    assertTrue(LazyList.continually(()).filter(_ => true).take(2) == Seq((), ()))
    assertTrue(LazyList.continually(()).filterNot(_ => false).take(2) == Seq((), ()))
    assertTrue(LazyList(1,2,3,4,5).filter(_ < 4) == Seq(1,2,3))
    assertTrue(LazyList(1,2,3,4,5).filterNot(_ > 4) == Seq(1,2,3,4))
    assertTrue(LazyList.from(1).filter(_ > 4).take(3) == Seq(5,6,7))
    assertTrue(LazyList.from(1).filterNot(_ <= 4).take(3) == Seq(5,6,7))
  }

  @Test // scala/bug#8990
  def withFilter_can_retry_after_exception_thrown_in_filter(): Unit = {
    // use mutable state to control an intermittent failure in filtering the LazyList
    var shouldThrow = true

    val wf = LazyList.from(1).take(10).withFilter { n =>
      if (shouldThrow && n == 5) throw new RuntimeException("n == 5") else n > 5
    }

    assertEquals(true, Try { wf.map(identity).length }.isFailure) // throws on n == 5

    shouldThrow = false                              // won't throw next time

    assertEquals(5,  wf.map(identity).length)       // success instead of NPE
  }

  @Test // scala/bug#6881
  def test_reference_equality(): Unit = {
    // Make sure we're tested with reference equality
    val s = LazyList.from(0)
    assert(s == s, "Referentially identical LazyLists should be equal (==)")
    assert(s equals s, "Referentially identical LazyLists should be equal (equals)")
    assert((0 #:: 1 #:: s) == (0 #:: 1 #:: s), "Cons of referentially identical LazyLists should be equal (==)")
    assert((0 #:: 1 #:: s) equals (0 #:: 1 #:: s), "Cons of referentially identical LazyLists should be equal (equals)")
  }

  @Test
  def t9886(): Unit = {
    assertEquals(LazyList(None, Some(1)), None #:: LazyList(Some(1)))
    assertEquals(LazyList(None, Some(1)), LazyList(None) #::: LazyList(Some(1)))
  }

  @Test
  def testLazyListDoesNotForceHead(): Unit = {
    var i = 0
    def f: Int = { i += 1; i }
    @unused val s = LazyList.empty.#::(f).#::(f).#::(f)
    assertEquals(0, i)
  }

  @Test
  def testEmptyLazyListToString(): Unit = {
    assertEquals("LazyList()", LazyList.empty.force.toString)
  }

  @Test
  def testLazyListToStringWhenHeadAndTailBothAreNotEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    assertEquals("LazyList(<not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhenOnlyHeadIsEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.head
    assertEquals("LazyList(1, <not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadAndTailIsEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.head
    l.tail
    assertEquals("LazyList(1, <not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadAndTailHeadIsEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.head
    l.tail.head
    assertEquals("LazyList(1, 2, <not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadIsNotEvaluatedAndOnlyTailIsEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail
    assertEquals("LazyList(1, <not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadIsNotEvaluatedAndTailHeadIsEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.head
    assertEquals("LazyList(1, 2, <not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhenHeadIsNotEvaluatedAndTailTailIsEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.tail
    assertEquals("LazyList(1, 2, <not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhendHeadIsNotEvaluatedAndTailTailHeadIsEvaluated(): Unit = {
    val l = LazyList(1, 2, 3, 4, 5)
    l.tail.tail.head
    assertEquals("LazyList(1, 2, 3, <not computed>)", l.toString)
  }

  @Test
  def testLazyListToStringWhenLazyListIsForcedToList(): Unit = {
    val l = 1 #:: 2 #:: 3 #:: 4 #:: LazyList.empty
    l.toList
    assertEquals("LazyList(1, 2, 3, 4)", l.toString)
  }

  @Test
  def testLazyListToStringWhenLazyListIsEmpty(): Unit = {
    // cached empty
    val l1 = LazyList.empty
    assertEquals("LazyList()", l1.toString)
    // non-cached empty
    val l2 = LazyList.unfold(0)(_ => None)
    assertEquals("LazyList(<not computed>)", l2.toString)
  }

  @Test
  def testLazyListToStringForSingleElementList(): Unit = {
    val l = LazyList(1)
    l.force
    assertEquals("LazyList(1)", l.toString)
  }

  @Test def toStringTailCycle(): Unit = {
    lazy val xs: LazyList[Int] = 1 #:: 2 #:: xs
    xs.tail.tail.head
    assertEquals("LazyList(1, 2, <cycle>)", xs.toString)
    assertEquals("LazyList(2, 1, <cycle>)", xs.tail.toString)
    assertEquals("LazyList(1, 2, <cycle>)", xs.tail.tail.toString)

    val ys = 0 #:: xs
    ys.tail.tail.tail.head
    assertEquals("LazyList(0, 1, 2, <cycle>)", ys.toString)
    assertEquals("LazyList(1, 2, <cycle>)", ys.tail.toString)
    assertEquals("LazyList(2, 1, <cycle>)", ys.tail.tail.toString)
  }

  @Test
  def testLazyListToStringWhenLazyListHasCyclicReference(): Unit = {
    lazy val cyc: LazyList[Int] = 1 #:: 2 #:: 3 #:: 4 #:: cyc
    assertEquals("LazyList(<not computed>)", cyc.toString)
    cyc.head
    assertEquals("LazyList(1, <not computed>)", cyc.toString)
    cyc.tail
    assertEquals("LazyList(1, <not computed>)", cyc.toString)
    cyc.tail.head
    assertEquals("LazyList(1, 2, <not computed>)", cyc.toString)
    cyc.tail.tail.head
    assertEquals("LazyList(1, 2, 3, <not computed>)", cyc.toString)
    cyc.tail.tail.tail.head
    assertEquals("LazyList(1, 2, 3, 4, <not computed>)", cyc.toString)
    cyc.tail.tail.tail.tail.head
    assertEquals("LazyList(1, 2, 3, 4, <cycle>)", cyc.toString)

    lazy val c1: LazyList[Int] = 1 #:: c1
    c1.tail.tail.tail
    assertEquals("LazyList(1, <cycle>)", c1.toString)
  }

  @Test
  def hasCorrectDrop(): Unit = {
    assertEquals(LazyList(), LazyList().drop(2))
    assertEquals(LazyList(), LazyList(1).drop(2))
    assertEquals(LazyList(), LazyList(1, 2).drop(2))
    assertEquals(LazyList(3), LazyList(1, 2, 3).drop(2))
    assertEquals(LazyList(3, 4), LazyList(1, 2, 3, 4).drop(2))
  }

  @Test
  def testForceReturnsEvaluatedLazyList() : Unit = {
    var i = 0
    def f: Int = { i += 1; i }
    val xs = LazyList.from(Iterator.fill(3)(f))
    assertEquals(0, i)
    xs.force
    assertEquals(3, i)
    // it's possible to implement `force` with incorrect string representation
    // (to forget about `tlEvaluated` update)
    assertEquals( "LazyList(1, 2, 3)", xs.toString())
  }

  @Test(timeout=10000)
  def testSameElements(): Unit = {
    object i {
      val cycle1: LazyList[Int] = 1 #:: 2 #:: cycle1
      val cycle2: LazyList[Int] = 1 #:: 2 #:: 3 #:: cycle2
    }
    import i._
    assert(LazyList().sameElements(LazyList()))
    assert(!LazyList().sameElements(LazyList(1)))
    assert(LazyList(1,2).sameElements(LazyList(1,2)))
    assert(!LazyList(1,2).sameElements(LazyList(1)))
    assert(!LazyList(1).sameElements(LazyList(1,2)))
    assert(!LazyList(1).sameElements(LazyList(2)))
    assert(cycle1.sameElements(cycle1))
    assert(!cycle1.sameElements(cycle2))
    assert(!cycle1.sameElements(cycle2))
  }

  @Test
  def toStringIsStackSafe(): Unit = {
    val l = LazyList.from(Range.inclusive(1, 10000))
    l.foreach(_ => ())
    @unused val s = l.toString // No exception thrown
  }

  @Test
  def laziness(): Unit = {
    lazy val fibs: LazyList[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
    assert(List(0, 1, 1, 2) == fibs.take(4).to(List))

    var lazeCount = 0
    def lazeL(i: Int) = { lazeCount += 1; i }
    @unused val xs21 = lazeL(1) #:: lazeL(2) #:: lazeL(3) #:: LazyList.empty

    assertEquals(0, lazeCount)
  }

  @Test  // Strawman issue #529
  def testLazyListMustComputeHeadOnlyOnce(): Unit = {
    var seedCounter = 0
    var fCounter = 0
    def seed(): Int = {
      seedCounter += 1
      1
    }
    val f: Int => Int = { x =>
      fCounter += 1
      x + 1
    }
    val xs = LazyList.iterate(seed())(f)
    assertEquals(0, seedCounter)
    assertEquals(0, fCounter)

    xs.head
    assertEquals(1, seedCounter)
    assertEquals(0, fCounter)

    xs.tail
    assertEquals(1, seedCounter)
    assertEquals(0, fCounter)

    xs.tail.head
    assertEquals(1, seedCounter)
    assertEquals(1, fCounter)

    xs.tail.tail
    assertEquals(1, seedCounter)
    assertEquals(1, fCounter)

    xs.tail.tail.head
    assertEquals(1, seedCounter)
    assertEquals(2, fCounter)

    xs.take(10).toList
    assertEquals(1, seedCounter)
    assertEquals(9, fCounter)
  }

  @Test
  def t8680(): Unit = {
    def pre(n: Int) = (-n to -1).to(LazyList)

    def cyc(m: Int) = {
      lazy val s: LazyList[Int] = (0 until m).to(LazyList) #::: s
      s
    }

    def precyc(n: Int, m: Int) = pre(n) #::: cyc(m)

    def goal(n: Int, m: Int) = (-n until m).mkString + "<cycle>"

    // Check un-forced cyclic and non-cyclic streams
    assertEquals("LazyList(<not computed>)", pre(2).toString)
    assertEquals("LazyList(<not computed>)", cyc(2).toString)
    assertEquals("LazyList(<not computed>)", precyc(2,2).toString)

    // Check forced cyclic and non-cyclic streams
    assertEquals("LazyList(-2, -1)", pre(2).force.toString)
    assertEquals("LazyList(0, 1, <cycle>)", cyc(2).force.toString)
    assertEquals("LazyList(-2, -1, 0, 1, <cycle>)", precyc(2,2).force.toString)

    // Special cases
    assertEquals("LazyList(0, <cycle>)", cyc(1).force.toString)
    assertEquals("LazyList(-1, 0, 1, 2, 3, 4, 5, <cycle>)", precyc(1,6).force.toString)
    assertEquals("LazyList(-6, -5, -4, -3, -2, -1, 0, <cycle>)", precyc(6,1).force.toString)

    // Make sure there are no odd/even problems
    for (n <- 3 to 4; m <- 3 to 4) {
      assertEquals(s"mkString $n $m", precyc(n,m).mkString, goal(n,m))
    }

    // Make sure there are no cycle/prefix modulus problems
    for (i <- 6 to 8) {
      assertEquals(s"mkString $i 3", goal(i,3), precyc(i,3).mkString)
      assertEquals(s"mkString 3 $i", goal(3,i), precyc(3,i).mkString)
    }
  }

  @Test
  def updated(): Unit = {
    val lazyList = LazyList from 0 take 4
    val list = lazyList.toList
    for (i <- lazyList.indices) {
      assertEquals(list.updated(i, -1), lazyList.updated(i, -1))
    }

    AssertUtil.assertThrows[IndexOutOfBoundsException](lazyList.updated(-1, -1))
  }

  @Test
  def tapEach(): Unit = {
    /** @param makeLL must make a lazylist that evaluates to Seq(1,2,3,4,5) */
    def check(makeLL: => LazyList[Int]): Unit = {
      val lb = ListBuffer[Int]()
      val ll = makeLL.tapEach(lb += _)
      assertEquals(ListBuffer[Int](), lb)
      assertEquals(Vector(1, 2), ll.take(2).to(Vector))
      assertEquals(ListBuffer(1, 2), lb)
      assertEquals(4, ll(3))
      assertEquals(ListBuffer(1, 2, 3, 4), lb)
      assertEquals(Vector(1, 2, 3, 4, 5), ll.to(Vector))
      assertEquals(ListBuffer(1, 2, 3, 4, 5), lb)
    }

    check(LazyList.from(Iterator(1, 2, 3, 4, 5)))
    check(LazyList.from(Vector(1, 2, 3, 4, 5)))
    check(LazyList.tabulate(5)(_ + 1))
  }

  @Test
  def builder(): Unit = {
    def build(init: Builder[Int, LazyList[Int]] => Unit): LazyList[Int] = {
      val b = LazyList.newBuilder[Int]
      init(b)
      b.result()
    }

    assertEquals(Nil, build(_ => ()))
    assertEquals(Nil, build(_ ++= Nil))
    assertEquals(Nil, build(_ ++= LazyList.empty))
    assertEquals(1 to 10, build(_ += 1 ++= (2 to 5) += 6 += 7 ++= (8 to 10)))
    assertEquals(1 to 10, build(_ ++= (1 to 4) ++= (5 to 6) += 7 ++= (8 to 9) += 10))
    assertEquals(1 to 10, build(_ ++= LazyList.from(1).take(10)))
    assertEquals(1 to 10, build(_ ++= Iterator.from(1).take(10)))
  }

  @Test
  def selfReferentialFailure(): Unit = {
    def assertNoStackOverflow[A](lazyList: LazyList[A]): Unit = {
      // don't hang the test if we've made a programming error in this test
      val finite = lazyList.take(1000)
      AssertUtil.assertThrows[RuntimeException](finite.force, _ contains "self-reference")
    }
    assertNoStackOverflow { class L { val ll: LazyList[Nothing] = LazyList.empty #::: ll }; (new L).ll }
    assertNoStackOverflow { class L { val ll: LazyList[Int] = 1 #:: ll.map(_ + 1).filter(_ % 2 == 0) }; (new L).ll }
    class L {
      lazy val a: LazyList[Nothing] = LazyList.empty #::: b
      lazy val b: LazyList[Nothing] = LazyList.empty #::: a
    }
    assertNoStackOverflow((new L).a)
    assertNoStackOverflow((new L).b)

    assertNoStackOverflow { object t { val ll: LazyList[Int] = 1 #:: ll.drop(1) }; t.ll }
  }

  // scala/bug#11931
  @Test
  def lazyAppendedAllExecutesOnce(): Unit = {
    var count = 0
    LazyList(1).lazyAppendedAll({ count += 1; Seq(2)}).toList
    assertEquals(1, count)
  }
}

object LazyListTest {
  var serializationForceCount = 0

  object sd {
    import java.io._

    def serialize(obj: AnyRef): Array[Byte] = {
      val buffer = new ByteArrayOutputStream
      val out = new ObjectOutputStream(buffer)
      out.writeObject(obj)
      buffer.toByteArray
    }

    def deserialize(a: Array[Byte]): AnyRef = {
      val in = new ObjectInputStream(new ByteArrayInputStream(a))
      in.readObject
    }

    def serializeDeserialize[T <: AnyRef](obj: T) = deserialize(serialize(obj)).asInstanceOf[T]
  }
}
