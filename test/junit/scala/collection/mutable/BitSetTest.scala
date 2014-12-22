package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{ Assert, Test }

@RunWith(classOf[JUnit4])
class BitSetTest {
  // Test for SI-8910
  @Test def capacityExpansionTest() {
    val bitSet = BitSet.empty
    val size   = bitSet.toBitMask.length
    bitSet ^= bitSet
    assert(bitSet.toBitMask.length == size, "Capacity of bitset changed after ^=")
    bitSet |= bitSet
    assert(bitSet.toBitMask.length == size, "Capacity of bitset changed after |=")
    bitSet &= bitSet
    assert(bitSet.toBitMask.length == size, "Capacity of bitset changed after &=")
    bitSet &~= bitSet
    assert(bitSet.toBitMask.length == size, "Capacity of bitset changed after &~=")
  }

  // Test for SI-7771
  @Test def testEquality(): Unit = {
    Assert.assertEquals(BitSet(1, 2, 3), BitSet(3, 2, 1))
    Assert.assertEquals(BitSet.empty, BitSet.empty)
    Assert.assertNotEquals(BitSet(1, 2, 3), BitSet(1, 4, 1))
    Assert.assertEquals(BitSet(1, 1, 1), BitSet(1))
    Assert.assertEquals(BitSet(3, 2, 3, 2), BitSet(2, 3))
  }
}
