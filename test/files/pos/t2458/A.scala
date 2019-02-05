
package p

// was: test/files/pos/t2458/A.scala:4: warning: imported `BitSet` is permanently hidden by definition of class BitSet in package p

import scala.collection.BitSet

trait A {
  val b: BitSet = new scala.collection.mutable.BitSet
}
