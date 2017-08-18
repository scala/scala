package strawman.collection.mutable

import strawman.collection.immutable.Range

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class BitSetTest {
  // Test for scala/bug#8910
  @Test def capacityExpansionTest(): Unit = {
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
  
  @Test def test_SI8917(): Unit = {
    val bigBitSet = BitSet(1, 100, 10000)
    val littleBitSet = BitSet(100)
    bigBitSet &= littleBitSet
    assert(!(bigBitSet contains 10000), "&= not applied to the full bitset")
    littleBitSet &= bigBitSet
    assert(littleBitSet.toBitMask.length < bigBitSet.toBitMask.length, "Needlessly extended the size of bitset on &=")
  }

  @Test def t10164(): Unit = {
    val bs = BitSet()
    val last = (bs ++ Range.inclusive(0, 128)).last  // Just needs not to throw
    assert(last == 128)
  }
}
