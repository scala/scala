package scala.collection.immutable

import org.junit._
import Assert._

import scala.collection.Seq.empty
import scala.language.postfixOps
import scala.tools.testing.AssertUtil._
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

  @Test def testforwardSliceEquals(): Unit = {

    def generateTestIter = (0 to 10).view iterator


    assertSameElements(empty toList,  generateTestIter slice (3, -1))
    assertSameElements(empty toList,  generateTestIter slice (0, 0))
    assertSameElements(empty,  generateTestIter slice (3, 3))
    assertSameElements(List(0) ,  generateTestIter slice (0, 1))
    assertSameElements(0 to 1 ,  generateTestIter slice (0, 2))
    assertSameElements(3 to 6 toList, generateTestIter slice(3,7))
    assertSameElements(0 to 2 toList, generateTestIter slice (-1, 3))
    assertSameElements(0 to 10 toList, generateTestIter slice (0, 11))
    assertSameElements(6 to 12 by 2 toList, generateTestIter slice (3, 7) map (2 * _))
    assertSameElements(6 to 12 by 2 toList, generateTestIter map (2 * _) slice (3, 7))
    assertSameElements(4 to 6 toList, generateTestIter slice (3, 7) drop 1)
    assertSameElements(4 to 7 toList, generateTestIter drop 1 slice (3, 7))
    assertSameElements(4 to 5 toList, generateTestIter slice (3, 7) slice (1, 3))
    assertSameElements(4 to 6 toList, generateTestIter slice (3, 7) slice (1, 10))
  }

  @Test def testbackwardSliceEquals(): Unit = {

    def generateTestIter = (0 to 10).view reverseIterator

    assertSameElements(empty toList,  generateTestIter slice (3, -1))
    assertSameElements(empty toList,  generateTestIter slice (3, 2))
    assertSameElements(empty,  generateTestIter slice (0, 0))
    assertSameElements(empty,  generateTestIter slice (3, 3))
    assertSameElements(List(10) ,  generateTestIter slice (0, 1))
    assertSameElements(10 to 9 by -1 ,  generateTestIter slice (0, 2))
    assertSameElements(7 to 4 by -1 toList, generateTestIter slice(3,7))
    assertSameElements(10 to 8 by -1 toList, generateTestIter slice (-1, 3))
    assertSameElements(14 to 8 by -2 toList, generateTestIter slice (3, 7) map (2 * _))
    assertSameElements(14 to 8 by -2 toList, generateTestIter map (2 * _) slice (3, 7))
    assertSameElements(6 to 4 by -1 toList, generateTestIter slice (3, 7) drop 1)
    assertSameElements(6 to 3 by -1 toList, generateTestIter drop 1 slice (3, 7))
    assertSameElements(6 to 5 by -1 toList, generateTestIter slice (3, 7) slice (1, 3))
    assertSameElements(6 to 4 by -1 toList, generateTestIter slice (3, 7) slice (1, 10))
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