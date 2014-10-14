object Test extends App {
  val bitset = new scala.collection.mutable.BitSet()
  val size   = bitset.toBitMask.length
  // Ensure the size of the BitSet doesn't change after certain operations.
  bitset ^= bitset
  assert(bitset.toBitMask.length == size, "Size of bitset changed after ^=")
  bitset |= bitset
  assert(bitset.toBitMask.length == size, "Size of bitset changed after |=")
  bitset &= bitset
  assert(bitset.toBitMask.length == size, "Size of bitset changed after &=")
  bitset &~= bitset
  assert(bitset.toBitMask.length == size, "Size of bitset changed after &~=")
}
