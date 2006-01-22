/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

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
      val newn = memsize(n);
      var newsize = (arr.length + 1) * 2; // works even in the case arr.length == 0
      while(newn > newsize)
        newsize = newsize * 2;
      val newarr = new Array[Int](newsize);
      arraycopy(arr, 0, newarr, 0, arr.length);
      arr = newarr;
      capacity = n;
    }

  /**
   * Sets <code>i<sup>th</sup></code> bit to true.
   * No restriction on <code>i</code>
   */
  def +=(i: Int): Unit = {
    ensureCapacity(i+1);
    if (!contains(i)) {
      arr(offset(i)) = arr(offset(i)) | mask(i);
      size = i + 1;
    }
  }

  /** Clears <code>i<sup>th</sup></code> bit  */
  def -=(i: Int): Unit =
    if (i < capacity && contains(i)) {
      arr(offset(i)) = arr(offset(i)) & ~mask(i);
      size = size - 1;
    }

  def clear: Unit = {
    java.util.Arrays.fill(arr, 0);
    size = 0;
  }

  def toImmutable: collection.immutable.BitSet =
    new immutable.BitSet(size, capacity, arr, true);

  var size: Int = 0;

  var capacity: Int = initSize;

  protected var arr: Array[Int] = new Array[Int](memsize(initSize));

}
