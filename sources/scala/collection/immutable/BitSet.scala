/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable ;

/** An immutable bitset view on a byte array. Instances can conveniently be
 *  created from instances of mutable.ResizableBitSet
 *  n:    number of relevant bits
 *  ba:   array of bytes of length n>>>3
 *  copy: if yes, then ba is copied and updates will not affect this bitset
 *  @author  Burak Emir
 */
class BitSet(n:Int, ba: Array[Byte], copy:Boolean) extends scala.collection.BitSet {

  final def size   = n;

  val array:Array[Byte]  =
    if( copy ) {
      val arr = new Array[Byte](ba.length);
      java.lang.System.arraycopy( ba, 0, arr, 0, ba.length );
      arr
    } else ba;

  def this(rbs: scala.collection.mutable.ResizableBitSet) = {
    this(rbs.size, rbs.toByteArray, false);
  }

  /** returns true if bit i is set */
  def apply(i: Int):Boolean = {
    val j    = (i >>> 3);
    val mask = (1 << (i & 0x07));
    (array(j) & mask) != 0;
  }

}
