/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable ;

/** mutable, resizable bit sets, to represent dense sets of small integers
 *  Bit indices are between 0..(size-1) inclusive
 *  @author  Burak Emir
 *  @param initSize: initial size in nbits
 */
class BitSet(initSize: Int) extends scala.collection.BitSet {

  /** default constructor, initial size of 32 bits */
  def this() = this( 32 );

  class ByteArray with ResizableArray[Int] {
    override protected val initialSize: Int = memsize( initSize );
    override protected var array: Array[Int] = new Array[Int](initialSize);

    /** size of this bitset in nbits */
    def ensureBits(nbits: Int): Unit = ensureSize(memsize( nbits ));

    final def and(j: Int, mask:Int): Unit = {
      array.update( j, array(j) & mask );
    }
    final def or(j: Int, mask:Int): Unit = {
      array.update( j, array(j) | mask );
    }
    def get(j:Int, mask:Int):Boolean = {
      (array(j) & mask) != 0;
    }

    def freeze: Array[Int] = {
      val arr = new Array[Int]( array.length );
      java.lang.System.arraycopy(array, 0, arr, 0, arr.length);
      arr
    }
  }

  protected val internal = new ByteArray();

  /** size of this bitset in nbytes */
  var size: Int = initSize;

  /** size of this bitset in nbits */
  def ensureSize(nbits: Int): Unit = {
    internal.ensureBits( nbits );
    if( size < nbits ) size = nbits;
  }

  /** calls set or clear for i-th bit */
  final def set(i: Int, b: Boolean): Unit = if( b ) set(i) else clear(i);

  /** sets i-th bit to true. Grows to size i+1 if needed. */
  final def set(i: Int): Unit = {
    ensureSize(i+1);
    val j         = (i >>> 5);
    val mask      = (1 << (i & 0x1F));
    internal.or(j, mask);
  }

  /** clears i-th bit. Grows to size i+1 if needed. */
  def clear(i: Int): Unit = {
    ensureSize(i+1);
    val j    = (i >>> 5);
    val mask = (1 << (i & 0x1F));
    internal.and(j, ~mask);
  }

  /** gets i-th bit. Grows to size i+1 if needed. */
  def apply(i: Int):Boolean = {
    val j    = (i >>> 5);
    val mask = (1 << (i & 0x1F));
    internal.get(j, mask);
  }

  def toArray: Array[Int] = internal.freeze;
}
