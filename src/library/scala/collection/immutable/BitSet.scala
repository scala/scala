/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


/** The class <code>BitSet</code>provides an immutable bitset view on an
 *  int array. Instances can conveniently be created from instances of
 *  Bit indices are between 0..(capacity-1) inclusive
 *
 *  @param size     <code>size</code> represents the number of relevant bits
 *  @param capacity ...
 *  @param ba       <code>ba</code> array of ints of length
 *                  <code>n&gt;&gt;&gt;5</code>
 *  @param copy     <code>copy</code> if yes, then <code>ba</code> is copied
 *                  and updates will not affect this bitset
 *
 *  @author  Burak Emir, Nikolay Mihaylov
 *  @version 1.0
 */

[serializable]
class BitSet(val size: Int, val capacity: Int, ba: Array[Int], copy: Boolean)
  extends collection.BitSet
{
  import compat.Platform.arraycopy

  protected val arr: Array[Int]  =
    if (copy) {
      val arr = new Array[Int](ba.length)
      arraycopy(ba, 0, arr, 0, ba.length)
      arr
    }
    else
      ba

}
