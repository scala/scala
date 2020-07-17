package scala.collection.immutable

import org.junit.Test
import org.junit.Assert._

import scala.collection.BitSetLikeTest

class BitSetTest extends BitSetLikeTest[BitSet] {
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
    val b2 = (companion.newBuilder ++= (l2) ).result

    assertEquals("List(ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, ffffffffffffffff, 1)",
                 b2.toBitMask.toList.map(_.toHexString).toString)
  }


}
