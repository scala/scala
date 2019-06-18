package scala.collection.mutable

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Assert._

import scala.tools.testkit.AssertUtil._

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

  @Test def test_SI8647(): Unit = {
    val bs = BitSet()
    bs.map(_ + 1)    // Just needs to compile
    val xs = bs: SortedSet[Int]
    xs.map(_ + 1)    // Also should compile (did before)
  }

  @Test def t10164(): Unit = {
    val bs = BitSet()
    val last = (bs ++ (0 to 128)).last  // Just needs not to throw
    assert(last == 128)
  }

  @Test def t10399(): Unit = {
    val bsFromEmptyBitMask = BitSet.fromBitMask(Array.empty[Long])
    assert(bsFromEmptyBitMask.add(0))
    val bsFromEmptyBitMaskNoCopy = BitSet.fromBitMaskNoCopy(Array.empty[Long])
    assert(bsFromEmptyBitMaskNoCopy.add(0))
  }

  @Test def strawman_508: Unit = {
    val m = BitSet(1)
    assert(m.map(i => i.toLong).isInstanceOf[TreeSet[Long]])
    assert(m.map(i => i + 1).isInstanceOf[BitSet])

    val im = collection.immutable.BitSet(1)
    assert(im.map(i=> i.toLong).isInstanceOf[collection.immutable.TreeSet[Long]])
    assert(im.map(i=> i + 1).isInstanceOf[collection.immutable.BitSet])

    // SI-10879
    assert(m.flatMap(i => Seq(i.toLong)).isInstanceOf[TreeSet[Long]])
    assert(m.flatMap(i => Seq(i + 1)).isInstanceOf[BitSet])
    assert(im.flatMap(i => Seq(i.toLong)).isInstanceOf[collection.immutable.TreeSet[Long]])
    assert(im.flatMap(i => Seq(i + 1)).isInstanceOf[collection.immutable.BitSet])
    assert(m.collect { case i => i.toLong }.isInstanceOf[TreeSet[Long]])
    assert(m.collect { case i => i + 1 }.isInstanceOf[BitSet])
    assert(im.collect { case i => i.toLong }.isInstanceOf[collection.immutable.TreeSet[Long]])
    assert(im.collect { case i => i + 1 }.isInstanceOf[collection.immutable.BitSet])
  }

  @Test def strawman_507: Unit = {
    val m = BitSet(1,2,3)
    assert(m.collect{case i if i%2 == 1 => i.toLong}.isInstanceOf[TreeSet[Long]])
    assert(m.collect{case i if i%2 == 1 => i.toLong} == TreeSet(1L, 3L))

    assert(m.collect{case i if i%2 == 1 => i}.isInstanceOf[BitSet])
    assert(m.collect{case i if i%2 == 1 => i} == BitSet(1, 3))

    val im = collection.immutable.BitSet(1,2,3)
    assert(im.collect{case i if i%2 == 1 => i.toLong}.isInstanceOf[collection.immutable.TreeSet[Long]])
    assert(im.collect{case i if i%2 == 1 => i.toLong} == collection.immutable.TreeSet(1L, 3L))

    assert(im.collect{case i if i%2 == 1 => i}.isInstanceOf[collection.immutable.BitSet])
    assert(im.collect{case i if i%2 == 1 => i} == collection.immutable.BitSet(1, 3))
  }

  @Test def concat(): Unit = {
    val a = BitSet(1, 2, 3)
    val b = BitSet(2, 4, 6)
    assert(a.concat(b) == BitSet(1, 2, 3, 4, 6))
    assert(a.union(b) == BitSet(1, 2, 3, 4, 6))
    assert(a.concat(BitSet()) == BitSet(1, 2, 3))
    assert(BitSet().concat(a) == BitSet(1, 2, 3))
    assert(BitSet().concat(BitSet()) == BitSet())
  }

  @Test def intersect(): Unit = {
    val a = BitSet(1, 2, 3)
    val b = BitSet(2, 4, 6)
    assert(a.intersect(b) == BitSet(2))
    assert(a.intersect(BitSet(4, 6)) == BitSet())
    assert(a.intersect(BitSet()) == BitSet())
    assert(BitSet().intersect(a) == BitSet())
    assert(BitSet().intersect(BitSet()) == BitSet())
  }

  @Test def diff(): Unit = {
    val a = BitSet(1, 2, 3)
    val b = BitSet(2, 4, 6)
    assertEquals(BitSet(1, 3), a.diff(b))
    assert(b.diff(a) == BitSet(4, 6))
    assert(a.diff(BitSet(4, 6)) == BitSet(1, 2, 3))
    assert(a.diff(BitSet()) == BitSet(1, 2, 3))
    assert(BitSet().diff(a) == BitSet())
    assert(BitSet().diff(BitSet()) == BitSet())
  }

  @Test def buildFromRange(): Unit = {
    import scala.util.chaining._
    assert((1 to 1000).to(BitSet) == BitSet().tap(bs => (1 to 1000).foreach(bs.addOne)))

  }
}
