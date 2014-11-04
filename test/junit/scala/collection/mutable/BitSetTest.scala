package scala.collection.mutable

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.{Test, Ignore}

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
}
