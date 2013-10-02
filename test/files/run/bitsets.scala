//############################################################################
// Bitsets
//############################################################################

//############################################################################

import scala.language.postfixOps

object TestMutable {
  import scala.collection.mutable.BitSet

  val ms0 = new BitSet
  val ms1 = new BitSet(8)
  val ms2 = new BitSet(0)
  ms0 += 2
  ms1 ++= List(1, 2)
  ms1 -= 1
  ms1 --= List(1)
  ms2(2) = true
  ms2(3) = false

  Console.println("ms0 = " + ms0)
  Console.println("ms1 = " + ms1)
  Console.println("ms2 = " + ms2)

  Console.println("mb0 = " + ms0.contains(-1))
  Console.println("mb1 = " + ms1.contains(2))
  Console.println("mb2 = " + ms2.contains(3))

  Console.println("xs0 = " + ms0.iterator.toList)
  Console.println("xs1 = " + ms1.iterator.toList)
  Console.println("xs2 = " + ms2.iterator.toList)

  Console.println("ma0 = " + ms0.toList)
  Console.println("ma1 = " + ms1.toList)
  Console.println("ma2 = " + ms2.toList)

  Console.println("mi0 = " + ms0.toImmutable)
  Console.println("mi1 = " + ms1.toImmutable)
  Console.println("mi2 = " + ms2.toImmutable)
  Console.println

  val N = 257
  val gen = 3
  val bs = BitSet((1 until N): _*)
  (1 until N).foldLeft(gen) {
    case (acc, i) =>
      assert(bs.size == N-i, s"Bad size for $bs, expected ${N-i} actual ${bs.size}")
      assert(!bs.isEmpty, s"Unexpected isEmpty for $bs")
      bs -= acc
      acc*gen % N
  }
  assert(bs.size == 0, s"Expected size == 0 for $bs")
  assert(bs.isEmpty, s"Expected isEmpty for $bs")
}

object TestMutable2 {
  import scala.collection.mutable.BitSet
  import scala.collection.immutable.TreeSet

  val l0 = 0 to 24 by 2 toList
  val l1 = (190 to 255 toList) reverse
  val l2 = (0 to 256 toList)
  val l3 = (1 to 200 by 2 toList) reverse
  val t0 = TreeSet(l0: _*)
  val t1 = TreeSet(l1: _*)
  val t2 = TreeSet(l2: _*)
  val t3 = TreeSet(l3: _*)
  val b0 = BitSet(l0: _*)
  val b1 = BitSet(l1: _*)
  val b2 = BitSet(l2: _*)
  val b3 = BitSet(l3: _*)

  println("m2_m0 = " + b0.toBitMask.toList.map(_.toBinaryString))
  println("m2_m2 = " + b2.toBitMask.toList.map(_.toHexString))
  println("m2_m0c = " + (BitSet.fromBitMask(b0.toBitMask) == b0))
  println("m2_m1c = " + (BitSet.fromBitMask(b1.toBitMask) == b1))
  println("m2_m2c = " + (BitSet.fromBitMask(b2.toBitMask) == b2))
  println("m2_m3c = " + (BitSet.fromBitMask(b3.toBitMask) == b3))
  println("m2_i0 = " + (t0 == b0))
  println("m2_i1 = " + (t1 == b1))
  println("m2_i2 = " + (t2 == b2))
  println("m2_i3 = " + (t3 == b3))
  println("m2_f0 = " + (t0.from(42) == b0.from(42)))
  println("m2_f1 = " + (t1.from(42) == b1.from(42)))
  println("m2_f2 = " + (t2.from(42) == b2.from(42)))
  println("m2_f3 = " + (t3.from(42) == b3.from(42)))
  println("m2_t0 = " + (t0.to(195) == b0.to(195)))
  println("m2_t1 = " + (t1.to(195) == b1.to(195)))
  println("m2_t2 = " + (t2.to(195) == b2.to(195)))
  println("m2_t3 = " + (t3.to(195) == b3.to(195)))
  println("m2_r0 = " + (t0.range(43,194) == b0.range(43,194)))
  println("m2_r1 = " + (t1.range(43,194) == b1.range(43,194)))
  println("m2_r2 = " + (t2.range(43,194) == b2.range(43,194)))
  println("m2_r3 = " + (t3.range(43,194) == b3.range(43,194)))
  println
}

object TestMutable3 {
  import scala.collection.mutable.BitSet

  val b0 = BitSet(5, 6)
  val b1 = BitSet(7)
  val b2 = BitSet(1, 5)
  val b3 = BitSet(6, 7)
  val b4 = BitSet(6, 7)

  b1 |= b0
  println(s"b1:$b1")
  b2 &= b0
  println(s"b2:$b2")
  b3 ^= b0
  println(s"b3:$b3")
  b4 &~= b0
  println(s"b4:$b4")
  b0 ^= b0 |= b1
  println(s"b0:$b0")
}

/***
The memory requirements here are way beyond
what a test should exercise.

object TestMutable4 {
  import scala.collection.mutable.BitSet

  val bMax = BitSet(Int.MaxValue)
  println(s"bMax:$bMax")
  bMax.foreach(println)

  val bLarge = BitSet(2000000001)
  println(s"bLarge:$bLarge")

  println(bMax == bLarge)
}
***/

object TestImmutable {
  import scala.collection.immutable.BitSet

  val is0 = BitSet()
  val is1 = BitSet.fromBitMask(Array())
  val is2 = BitSet.fromBitMask(Array(4))
  val is3 = BitSet.empty

  Console.println("is0 = " + is0)
  Console.println("is1 = " + is1)
  Console.println("is2 = " + is2)
  Console.println("is3 = " + is3)

  Console.println("ib0 = " + is0.contains(-1))
  Console.println("ib1 = " + is1.contains(0))
  Console.println("ib2 = " + is2.contains(2))
  Console.println("ib3 = " + is3.contains(2))

  Console.println("ys0 = " + is0.iterator.toList)
  Console.println("ys1 = " + is1.iterator.toList)
  Console.println("ys2 = " + is2.iterator.toList)
  Console.println("ys3 = " + is3.iterator.toList)

  Console.println("ia0 = " + is0.toList)
  Console.println("ia1 = " + is1.toList)
  Console.println("ia2 = " + is2.toList)
  Console.println("ia3 = " + is3.toList)
  Console.println
}

object TestImmutable2 {
  import scala.collection.immutable.{BitSet, TreeSet}

  val l0 = 0 to 24 by 2 toList
  val l1 = (190 to 255 toList) reverse
  val l2 = (0 to 256 toList)
  val l3 = (1 to 200 by 2 toList) reverse
  val t0 = TreeSet(l0: _*)
  val t1 = TreeSet(l1: _*)
  val t2 = TreeSet(l2: _*)
  val t3 = TreeSet(l3: _*)
  val b0 = BitSet(l0: _*)
  val b1 = BitSet(l1: _*)
  val b2 = BitSet(l2: _*)
  val b3 = BitSet(l3: _*)

  println("i2_m0 = " + b0.toBitMask.toList.map(_.toBinaryString))
  println("i2_m2 = " + b2.toBitMask.toList.map(_.toHexString))
  println("i2_m0c = " + (BitSet.fromBitMask(b0.toBitMask) == b0))
  println("i2_m1c = " + (BitSet.fromBitMask(b1.toBitMask) == b1))
  println("i2_m2c = " + (BitSet.fromBitMask(b2.toBitMask) == b2))
  println("i2_m3c = " + (BitSet.fromBitMask(b3.toBitMask) == b3))
  println("i2_i0 = " + (t0 == b0))
  println("i2_i1 = " + (t1 == b1))
  println("i2_i2 = " + (t2 == b2))
  println("i2_i3 = " + (t3 == b3))
  println("i2_f0 = " + (t0.from(42) == b0.from(42)))
  println("i2_f1 = " + (t1.from(42) == b1.from(42)))
  println("i2_f2 = " + (t2.from(42) == b2.from(42)))
  println("i2_f3 = " + (t3.from(42) == b3.from(42)))
  println("i2_t0 = " + (t0.to(195) == b0.to(195)))
  println("i2_t1 = " + (t1.to(195) == b1.to(195)))
  println("i2_t2 = " + (t2.to(195) == b2.to(195)))
  println("i2_t3 = " + (t3.to(195) == b3.to(195)))
  println("i2_r0 = " + (t0.range(77,194) == b0.range(77,194)))
  println("i2_r1 = " + (t1.range(77,194) == b1.range(77,194)))
  println("i2_r2 = " + (t2.range(77,194) == b2.range(77,194)))
  println("i2_r3 = " + (t3.range(77,194) == b3.range(77,194)))
  println
}

object Test extends App {
  TestMutable
  TestMutable2
  TestMutable3
  // TestMutable4
  TestImmutable
  TestImmutable2
}

//############################################################################
