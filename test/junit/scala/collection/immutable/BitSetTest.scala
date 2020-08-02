package scala.collection.immutable

import org.junit.Test
import org.junit.Assert._

import scala.collection.{BitSetLikeTest, mutable}
import scala.tools.testing.AllocationTest

class BitSetTest extends BitSetLikeTest[BitSet] with AllocationTest {
  override def companion = BitSet
  override def fromBitMask(elems: Array[Long]): BitSet = BitSet.fromBitMask(elems)

  @Test def applyTrimmedToSize: Unit = {
    val l2 = (0 to 256 toList)
    val b2 = companion(l2: _*)

    assertEquals("List(ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, 1)",
                 b2.toBitMask.toList.map(_.toHexString).toString)
  }
  @Test def builderTrimmedToSize: Unit = {
    val l2 = (0 to 256 toList)
    val b2 = (companion.newBuilder ++= (l2)).result

    assertEquals("List(ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, 1)",
                 b2.toBitMask.toList.map(_.toHexString).toString)
  }

  @Test def shrinking: Unit = {
    val b3 = BitSet(0, 64, 128)
    assertTrue(b3.isInstanceOf[BitSet.BitSetN])
    val b2 = b3 - 128
    assertTrue(b2.isInstanceOf[BitSet.BitSet2])
    val b1 = b2 - 64
    assertTrue(b1.isInstanceOf[BitSet.BitSet1])
    val b0 = b1 - 0
    assertSame(BitSet.empty, b0)
  }

  @Test def noopAdditions: Unit = {
    for (ds <- datasets) {
      val bs = (companion.newBuilder ++= ds).result
      assertSame(bs, nonAllocating(bs ++ BitSet.empty))
      if (bs nonEmpty) {
        assertSame(bs, nonAllocating(bs + bs.head))
        assertSame(bs, nonAllocating(bs + bs.last))
      }
      for (ss <- bs.subsets()) {

        assertSame(bs, nonAllocating(bs ++ ss))

        val ssm = new mutable.BitSet ++ ss
        assertSame(bs, nonAllocating(bs ++ ssm))

        val ssl = ss.toList
        assertSame(bs, nonAllocating(bs ++ ssl))

        val sst = ss.to[TreeSet]
        assertSame(bs, onlyAllocates(64)(bs ++ sst))
      }
    }
  }
  @Test def noopRemovals: Unit = {
    for (ds <- datasets) {
      val bs = (companion.newBuilder ++= ds).result
      assertSame(bs, nonAllocating(bs -- BitSet.empty))
      val outside = companion(bs.headOption.getOrElse(0) + 1)
      assertSame(bs, nonAllocating(bs - outside.head))

      assertSame(bs, nonAllocating(bs -- outside))

      val osm = new mutable.BitSet ++ outside
      assertSame(bs, nonAllocating(bs -- osm))

      val osl = outside.toList
      assertSame(bs, nonAllocating(bs -- osl))

      val ost = outside.to[TreeSet]
      assertSame(bs, onlyAllocates(64)(bs -- ost))
    }
  }
  @Test def allocateApply: Unit = {
    assertSame(BitSet.empty, nonAllocating(BitSet.empty))
    assertSame(BitSet.empty, nonAllocating(BitSet()))

    def check(allocation: Int, values: Int*): Unit = {
      var params: collection.Seq[Int] = mutable.WrappedArray.make(Array(values: _*))
      onlyAllocates(allocation)(BitSet(params: _*))
      params = params.toList
      onlyAllocates(allocation)(BitSet(params: _*))
    }

    check(24, 0)
    check(24, 0, 10, 50, 63)
    check(32, 0, 10, 50, 63, 64)
    check(32, 64)
    check(32, 64, 127)
    check(32, 127)
    check(56, 128) //16 BitSitN, 16 Array Header, 24 Array data
    check(200, 1280) //16 BitSitN, 16 Array Header, 168 Array data
    check(200, 1, 100, 200, 30, 400, 500, 600, 2, 999, 1000, 1280)
  }
}
