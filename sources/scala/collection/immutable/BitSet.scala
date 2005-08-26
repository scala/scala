/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.immutable;

/** The class <code>BitSet</code>provides an immutable bitset view on an
 *  int array. Instances can conveniently be created from instances of
 *  <code>mutable.ResizableBitSet</code>. Bit indices are between 0..(size-1) inclusive
 *
 *  @param <code>n</code> represents the number of relevant bits
 *  @param ba:   array of ints of length <code>n</code>&gt;&gt;&gt;5
 *  @param copy: if yes, then <code>ba</code> is copied and updates will
 *               not affect this bitset
 *
 *  @author  Burak Emir
 *  @version 1.0
 */
[serializable]
class BitSet(n:Int, ba: Array[Int], copy: Boolean) extends collection.BitSet with Ordered[BitSet] {

  /** lexicographic ordering */
  def compareTo [b >: BitSet <% Ordered[b]](other: b): int = other match {
    case that:BitSet =>
      val it1 = this.toSet(true).elements;
      val it2 = that.toSet(true).elements;
      var res = 0;
      while((0==res) && it1.hasNext) {
        while((0==res) && it2.hasNext) {
          if(!it1.hasNext)
            res = -1
          else {
            val i1 = it1.next;
            val i2 = it2.next;
            if(i1<i2)
              res = -1
            else if(i1>i2)
              res = 1
          }
        }
        if(it1.hasNext)
          res = 1
      }
      if(it2.hasNext)
        res = -1;
      res

    case _ => -(other.compareTo(this))
  }


  final def size = n;

  protected val array: Array[Int]  =
    if (copy) {
      val arr = new Array[Int](ba.length);
      java.lang.System.arraycopy(ba, 0, arr, 0, ba.length);
      arr
    }
    else
      ba;

  /**
   * Checks if two bitsets are structurally identical.
   *
   *  @return true, iff both bitsets contain the same sequence of elements.
   */
  override def equals(that: Any): Boolean =
    that.isInstanceOf[BitSet] &&
    { val other = that.asInstanceOf[BitSet];
      (size == other.size) && ( size == 0 || {
        var len = memsize( size );
        var i = 0;
        var res=true;
        while(( i< len ) && res ) {        // 32 x faster equality check
          res = array(i) == other.array(i);
          i = i + 1;
        }
        res
      })
   } || super.equals(that);

  def this(rbs: mutable.BitSet) = {
    this(rbs.size, rbs.toArray, false);
  }

  /** returns true if bit i is set
   *
   * @param i
   */
  def apply(i: Int):Boolean = {
    val j    = (i >>> 5);
    val mask = (1 << (i & 0x1F));
    (array(j) & mask) != 0;
  }

  def makeMutable = {
    val rbs = new mutable.BitSet(size) ;
    val it = this.toSet(true).elements;
    while(it.hasNext) {
      rbs.set(it.next)
    }
    rbs
  }

  def toArray: Array[Int] = {
    val arr = new Array[Int](array.length);
    java.lang.System.arraycopy(arr, 0, array, 0, array.length);
    arr
  }
}
