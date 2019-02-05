package scala.util.hashing

import scala.collection.immutable.ArraySeq
import org.junit.Assert._
import org.junit.Test

class MurmurHash3Test {

  def check(h: Int, a: Array[Int]): Unit = {
    assertEquals(h, ArraySeq.unsafeWrapArray(a).hashCode)
    assertEquals(h, MurmurHash3.arrayHash(a, MurmurHash3.seqSeed))
    assertEquals(h, MurmurHash3.listHash(a.toList, MurmurHash3.seqSeed))
    assertEquals(h, MurmurHash3.orderedHash(a.toList, MurmurHash3.seqSeed))
  }

  def swap(a: Array[Int], i1: Int, i2: Int): Unit = {
    val tmp = a(i1)
    a(i1) = a(i2)
    a(i2) = tmp
  }

  @Test
  def testRangeConsistency: Unit = for(size <- Seq(0, 1, 2, 3, 4, 10, 100)) {
    val range = (1 to size)
    val ordered = Array.iterate(1, size)(_ + 1)
    assertEquals(range, ArraySeq.unsafeWrapArray(ordered))
    check(range.hashCode, ordered)
  }

  @Test
  def testNonRangeConsistency: Unit = for(size <- Seq(2, 3, 4, 10, 100)) {
    val range = (1 to size)
    val ordered = Array.iterate(1, size)(_ + 1)
    val mixed1 = Array.copyOf(ordered, ordered.length)
    val mixed2 = Array.copyOf(ordered, ordered.length)
    swap(mixed1, 0, 1)
    swap(mixed2, mixed2.length-1, mixed2.length-2)
    check(ArraySeq.unsafeWrapArray(mixed1).hashCode, mixed1)
    check(ArraySeq.unsafeWrapArray(mixed2).hashCode, mixed2)
  }

  @Test
  def testRangeAlignment: Unit = {
    // 2 elements: end is normalized
    val r1 = 1 to 3 by 2
    val r2 = 1 to 4 by 2
    assertEquals(r1, r2)
    assertEquals(r1.hashCode, r2.hashCode)
    // > 2 elements: end is normalized
    val r3 = 1 to 9 by 2
    val r4 = 1 to 10 by 2
    assertEquals(r3, r4)
    assertEquals(r3.hashCode, r4.hashCode)
    // 1 element: end and step are ignored
    val r5 = 1 to 1 by 2
    val r6 = 1 to 5 by 17
    assertEquals(r5, r6)
    assertEquals(r5.hashCode, r6.hashCode)
  }
}
