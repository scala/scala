/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable;


/**
 * This class implements mutable, resizable Bit sets
 *
 * @author  Burak Emir, Nikolay Mihaylov
 * @version 1.1
 *
 * @param initSize: initial size in bits
 */

[serializable]
class BitSet(initSize: Int) extends collection.BitSet with mutable.Set[Int] {

  import scala.runtime.compat.Platform.arraycopy;

  /** default constructor, initial size of 512 bits */
  def this() = this(0);

  /** ensure that this bitset can store at least <pre>n</pre> bits */
  def ensureCapacity(n: Int): Unit =
    if (capacity < n) {
      if (nbits(arr.length) < n) {
        val newn = memsize(n);
        var newsize = if (arr.length == 0) newn else arr.length * 2;
        while (newn > newsize)
          newsize = newsize * 2;
        val newarr = new Array[Int](newsize);
        arraycopy(arr, 0, newarr, 0, arr.length);
        arr = newarr;
      }
      capacity = n;
    }

  /**
   * Sets <code>i<sup>th</sup></code> bit to true.
   * No restriction on <code>i</code>
   */
  def +=(i: Int): Unit = {
    ensureCapacity(i+1);
    val oldInt = arr(offset(i));
    val newInt = oldInt | mask(i);
    if (oldInt != newInt) {
      arr(offset(i)) = newInt;
      size = size + 1;
    }
  }

  /** Clears <code>i<sup>th</sup></code> bit  */
  def -=(i: Int): Unit = {
    val oldInt = arr(offset(i));
    val newInt = oldInt & ~mask(i);
    if (oldInt != newInt) {
      arr(offset(i)) = newInt;
      size = size - 1;
    }
  }

  def clear: Unit = {
    java.util.Arrays.fill(arr, 0);
    size = 0;
  }

  def toImmutable: collection.immutable.BitSet =
    new immutable.BitSet(size, capacity, arr, true);

  override def clone(): BitSet = new BitSet(capacity) {
    arraycopy(BitSet.this.arr, 0, arr, 0, arr.length);
    size = BitSet.this.size;
    capacity = BitSet.this.capacity;
  }

  var size: Int = 0;

  var capacity: Int = initSize;

  protected var arr: Array[Int] = new Array[Int](memsize(initSize));

}
