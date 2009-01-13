/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/**
 * The class <code>BitSet</code> implements mutable, resizable Bit sets
 *
 * @author  Burak Emir, Nikolay Mihaylov
 * @version 1.1
 *
 * @param initSize: initial size in bits
 */

@serializable
class BitSet(initSize: Int) extends collection.BitSet with Set[Int] {

  import compat.Platform.{arraycopy, arrayclear}

  /** default constructor, initial size of 512 bits. */
  def this() = this(0)

  /** Ensures that this bitset can store at least <code>n</code> bits.
   *
   *  @param n ...
   */
  def ensureCapacity(n: Int): Unit =
    if (capacity < n) {
      if (nbits(arr.length) < n) {
        val newn = memsize(n)
        var newsize = if (arr.length == 0) newn else arr.length * 2
        while (newn > newsize)
          newsize = newsize * 2;
        val newarr = new Array[Int](newsize)
        arraycopy(arr, 0, newarr, 0, arr.length)
        arr = newarr
      }
      capacity = n
    }

  /**
   * Sets <code>i-th</code> bit to true.
   * No restriction on <code>i</code>
   */
  def +=(i: Int): Unit = {
    ensureCapacity(i+1)
    val oldInt = arr(offset(i))
    val newInt = oldInt | mask(i)
    if (oldInt != newInt) {
      arr(offset(i)) = newInt
      size = size + 1
    }
  }

  /** Clears the <code>i</code>-th bit.
   *
   *  @param i the <code>i</code>-th element of the bit set.
   */
  def -=(i: Int): Unit = {
    if (i >= capacity) return;
    val oldInt = arr(offset(i))
    val newInt = oldInt & ~mask(i)
    if (oldInt != newInt) {
      arr(offset(i)) = newInt
      size = size - 1
    }
  }

  /** Clears all bits of the set.
   */
  override def clear(): Unit = {
    arrayclear(arr)
    size = 0
  }

  def toImmutable: collection.immutable.BitSet =
    new immutable.BitSet(size, capacity, arr, true)

  override def clone(): BitSet = new BitSet(capacity) {
    arraycopy(BitSet.this.arr, 0, arr, 0, arr.length)
    size = BitSet.this.size
    capacity = BitSet.this.capacity
  }

  var size: Int = 0

  var capacity: Int = initSize

  protected var arr: Array[Int] = new Array[Int](memsize(initSize))

}
