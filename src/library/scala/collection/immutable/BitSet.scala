/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

/** The class <code>BitSet</code>provides an immutable bitset view on an
 *  int array. Instances can conveniently be created from instances of
 *  Bit indices are between 0..(capacity-1) inclusive
 *
 *  @param <code>n</code> represents the number of relevant bits
 *  @param ba:   array of ints of length <code>n</code>&gt;&gt;&gt;5
 *  @param copy: if yes, then <code>ba</code> is copied and updates will
 *               not affect this bitset
 *
 *  @author  Burak Emir, Nikolay Mihaylov
 *  @version 1.0
 */

[serializable]
class BitSet(val size: Int, val capacity: Int, ba: Array[Int], copy: Boolean)
  extends collection.BitSet
{
  import scala.runtime.compat.Platform.arraycopy;

  protected val arr: Array[Int]  =
    if (copy) {
      val arr = new Array[Int](ba.length);
      arraycopy(ba, 0, arr, 0, ba.length);
      arr
    }
    else
      ba;

}

object BitSet {
  implicit def toOrdered(bs: BitSet): Ordered[BitSet] = new Ordered[BitSet] {
    def compareTo [b >: BitSet <% Ordered[b]](other: b): Int = other match {
      case that: BitSet => {
        val it1 = bs.elements;
        val it2 = that.elements;
        var res = 0;
        while((0 == res) && it1.hasNext) {
          while((0 == res) && it2.hasNext) {
            if (!it1.hasNext)
              res = -1
            else {
              val i1 = it1.next;
              val i2 = it2.next;
              if (i1 < i2)
                res = -1
              else if (i1 > i2)
                res = 1
            }
          }
          if (it1.hasNext)
            res = 1
        }
        if (it2.hasNext)
          res = -1;
        res
      }

      //case _ => -(other.compareTo(this))
    }
  }
}
