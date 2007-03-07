//############################################################################
// Bitsets
//############################################################################
// $Id$

//############################################################################

object Test extends Application {
  import scala.collection.mutable.{BitSet => MBitSet}
  import scala.collection.immutable.BitSet

  val s0 = new BitSet(8, 8, null, false)
  val s1 = new MBitSet(8)
  val s2 = new BitSet(8, 8, Array(4), false)
  s1 += 2

  Console.println("s0 = " + s0)
  Console.println("s1 = " + s1)
  Console.println("s2 = " + s2)

  Console.println("xs0 = " + s0.elements.toList)
  Console.println("xs1 = " + s1.elements.toList)
  Console.println("xs2 = " + s2.elements.toList)

  Console.println("ys0 = " + s0.toArray.toList)
  Console.println("ys1 = " + s1.toArray.toList)
  Console.println("ys2 = " + s2.toArray.toList)
}

//############################################################################
