/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable


/** The class <code>BitSet</code> provides an immutable bitset view on an
 *  int array. Instances can conveniently be created from instances of
 *  Bit indices are between <code>0..(capacity-1)</code> inclusive.
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

@serializable
/*
 *                This is a strange class! It claims to be immutable but is not.
 *                It claims to be a BitSet but it is not a Set.
 *                Remove it or integrate it into the Set hierarchy.
 *                [Comments by Martin]
 */
class BitSet(val size: Int, val capacity: Int, ba: Array[Int], copy: Boolean)
  extends collection.BitSet
{
  import compat.Platform.arraycopy

  protected val arr: Array[Int] = {
    val ba1 = if (ba != null) ba else new Array[Int](0)
    if (copy) {
      val arr = new Array[Int](ba1.length)
      arraycopy(ba1, 0, arr, 0, ba1.length)
      arr
    }
    else
      ba1
  }

}
