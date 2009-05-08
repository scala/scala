//############################################################################
// Bitsets
//############################################################################
// $Id$

//############################################################################

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

  Console.println("xs0 = " + ms0.elements.toList)
  Console.println("xs1 = " + ms1.elements.toList)
  Console.println("xs2 = " + ms2.elements.toList)

  Console.println("ma0 = " + ms0.underlying.toList)
  Console.println("ma1 = " + ms1.underlying.toList)
  Console.println("ma2 = " + ms2.underlying.toList)

  Console.println("mi0 = " + ms0.toImmutable)
  Console.println("mi1 = " + ms1.toImmutable)
  Console.println("mi2 = " + ms2.toImmutable)
  Console.println
}

object TestImmutable {
  import scala.collection.immutable.BitSet

  val is0 = new BitSet(8, 1, null, false)
  val is1 = new BitSet(8, 1, Array(), false)
  val is2 = new BitSet(8, 8, Array(4), false)
  val is3 = new BitSet(0, 0, null, false)

  Console.println("is0 = " + is0)
  Console.println("is1 = " + is1)
  Console.println("is2 = " + is2)
  Console.println("is3 = " + is3)

  Console.println("ib0 = " + is0.contains(-1))
  Console.println("ib1 = " + is1.contains(0))
  Console.println("ib2 = " + is2.contains(2))
  Console.println("ib3 = " + is3.contains(2))

  Console.println("ys0 = " + is0.elements.toList)
  Console.println("ys1 = " + is1.elements.toList)
  Console.println("ys2 = " + is2.elements.toList)
  Console.println("ys3 = " + is3.elements.toList)

  Console.println("ia0 = " + is0.underlying.toList)
  Console.println("ia1 = " + is1.underlying.toList)
  Console.println("ia2 = " + is2.underlying.toList)
  Console.println("ia3 = " + is3.underlying.toList)
  Console.println
}

object Test extends Application {
  TestMutable
  TestImmutable
}

//############################################################################
