package scala.collection.immutable

import org.junit._
import Assert._

import scala.util.AllocationTest

class IndexedSeqTest extends AllocationTest {

  @Test def testEqualsSimple: Unit = {
    assertTrue(Vector.empty == Vector.empty)
    assertTrue(Vector(1, 2, 3) == Vector(1, 2, 3))
    assertTrue(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9) == Vector(1, 2, 3, 4, 5, 6, 7, 8, 9))

    assertTrue(ArraySeq.empty == ArraySeq.empty)
    assertTrue(ArraySeq(1, 2, 3) == ArraySeq(1, 2, 3))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) == ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  @Test def testNotEqualsSimple1: Unit = {
    assertTrue(Vector.empty != Vector(1, 2, 3))
    assertTrue(Vector(3, 2, 1) != Vector(1, 2, 3))
    assertTrue(Vector(1, 2, 3) != Vector(1, 2))
    assertTrue(Vector(1, 2) != Vector(1, 2, 3))
    assertTrue(Vector(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 4, 5, 6, 7, 8, 99))
  }

  @Test def testNotEqualsSimple2: Unit = {
    assertTrue(ArraySeq.empty != ArraySeq(1, 2, 3))
    assertTrue(ArraySeq(3, 2, 1) != ArraySeq(1, 2, 3))
    assertTrue(ArraySeq(1, 2, 3) != ArraySeq(1, 2))
    assertTrue(ArraySeq(1, 2) != ArraySeq(1, 2, 3))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 99))
  }

  @Test def testNotEqualsSimple3: Unit = {
    assertTrue(ArraySeq.empty != Vector(1, 2, 3))
    assertTrue(ArraySeq(3, 2, 1) != Vector(1, 2, 3))
    assertTrue(ArraySeq(1, 2, 3) != Vector(1, 2))
    assertTrue(ArraySeq(1, 2) != Vector(1, 2, 3))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 4, 5, 6, 7, 8, 99))
  }

  @Test def testNotEqualsEdges: Unit = {
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(10, 2, 3, 4, 5, 6, 7, 8, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 20, 3, 4, 5, 6, 7, 8, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 30, 4, 5, 6, 7, 8, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 40, 5, 6, 7, 8, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 4, 50, 6, 7, 8, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 4, 5, 60, 7, 8, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 4, 5, 6, 70, 8, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 4, 5, 6, 7, 80, 9))
    assertTrue(ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9) != Vector(1, 2, 3, 4, 5, 6, 7, 8, 90))
  }

  @Test def testEqualsSimpleNonAllocatingEmpty: Unit = {
    nonAllocatingEqual(true, Vector.empty, Vector.empty)
    nonAllocatingEqual(true, ArraySeq.empty, ArraySeq.empty)
    nonAllocatingEqual(true, Vector.empty, ArraySeq.empty)
  }

  @Test def testEqualsSimpleNonAllocatingSmall: Unit = {
    nonAllocatingEqual(true, Vector(1, 2, 3), Vector(1, 2, 3))
    nonAllocatingEqual(true, ArraySeq(1, 2, 3), ArraySeq(1, 2, 3))
    nonAllocatingEqual(true, Vector(1, 2, 3), ArraySeq(1, 2, 3))
  }

  @Test def testEqualsSimpleNonAllocatingDiffSize: Unit = {
    nonAllocatingEqual(false, Vector(1, 2, 3, 4, 5, 6, 7, 8, 9), Vector(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    nonAllocatingEqual(false, ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    nonAllocatingEqual(false, Vector(1, 2, 3, 4, 5, 6, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  }

  @Test def testEqualsSimpleNonAllocatingDiffInFirstFew: Unit = {
    nonAllocatingEqual(false, Vector(1, 2, 3, 14, 5, 6, 7, 8, 9), Vector(1, 2, 3, 4, 5, 6, 7, 8, 9))
    nonAllocatingEqual(false, ArraySeq(1, 2, 3, 14, 5, 6, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9))
    nonAllocatingEqual(false, Vector(1, 2, 3, 14, 5, 6, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }

  @Test def testEqualsArraySeqSpecialised1: Unit = {
    nonAllocatingEqual(true, ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9), ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
  @Test def testEqualsArraySeqSpecialised2: Unit = {
    nonAllocatingEqual(true, ArraySeq(1, 2, 3, 4, 5, 6, 7, 8, 9), Range(1,10))
  }

  @Test def testReflexiveEquality(): Unit = {
    assertNotEquals(new MyRange(0, 1), List(0))
    assertNotEquals(new MyRange(0, 1), Vector(0))
    assertNotEquals(new MyRange(0, 1), Range(0,1))
    assertNotEquals(List(0), new MyRange(0, 1))
    assertNotEquals(Vector(0), new MyRange(0, 1))
    assertNotEquals(Range(0,1), new MyRange(0, 1))
    assertEquals(new MyRange(0, 1), new MyRange(0, 1))
  }
}
final class MyRange(val start: Int, val end: Int) extends IndexedSeq[Int] {
  def apply(idx: Int) = start + idx
  def length = end - start
  override def canEqual(other: Any) = other.isInstanceOf[MyRange]
  override def equals(other: Any) = other match {
    case other: MyRange => other.start == start && other.end == end
    case _ => false
  }
  override def hashCode = start + end
}