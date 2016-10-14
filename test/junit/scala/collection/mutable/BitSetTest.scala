package scala.collection.mutable

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

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
  
  @Test def test_SI8917() {
    val bigBitSet = BitSet(1, 100, 10000)
    val littleBitSet = BitSet(100)
    bigBitSet &= littleBitSet
    assert(!(bigBitSet contains 10000), "&= not applied to the full bitset")
    littleBitSet &= bigBitSet
    assert(littleBitSet.toBitMask.length < bigBitSet.toBitMask.length, "Needlessly extended the size of bitset on &=")
  }

  @Test def test_SI8647() {
    val bs = BitSet()
    bs.map(_ + 1)    // Just needs to compile
    val xs = bs: SortedSet[Int]
    xs.map(_ + 1)    // Also should compile (did before)
  }
}
