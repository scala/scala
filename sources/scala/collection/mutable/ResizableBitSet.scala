/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.collection.mutable ;

/** resizable bit sets, to represent small sets of integers
 *  @author  Burak Emir
 *  @param initSize: initial size in nbits
 */
class ResizableBitSet(initSize: Int) extends scala.collection.BitSet {

  /** default constructor, initial size of 16 bits */
  def this() = this( 16 );

  class ByteArray with ResizableArray[Byte] {
    override protected val initialSize: Int = initSize >>> 3;
    override protected var array: Array[Byte] = new Array[Byte](initialSize);

    /** size of this bitset in nbits */
    def ensureBits(nbits: Int): Unit = ensureSize(nbits >>> 3);

    final def and(j: Int, mask:Int): Unit = {
      array.update( j, (array(j) & mask).asInstanceOf[Byte] );
    }
    final def or(j: Int, mask:Int): Unit = {
      array.update( j, (array(j) | mask).asInstanceOf[Byte] );
    }
    def get(j:Int, mask:Int):Boolean = {
      (array(j) & mask) != 0;
    }

    def freeze: Array[Byte] = {
      val arr = new Array[Byte]( array.length );
      java.lang.System.arraycopy(array, 0, arr, 0, arr.length);
      arr
    }
  }

  protected val internal = new ByteArray();

  /** size of this bitset in nbytes */
  var size: Int = 0;

  /** size of this bitset in nbits */
  def ensureSize(nbits: Int): Unit = {
    internal.ensureBits( nbits );
    size = nbits;
  }

  final def set(i: Int, b: Boolean): Unit = if( b ) set(i) else clear(i);

  final def set(i: Int): Unit = {
    val j         = (i >>> 3);
    val mask      = (1 << (i & 0x07));
    internal.or(j, mask);
  }

  def clear(i: Int): Unit = {
    val j    = (i >>> 3);
    val mask = (1 << (i & 0x07));
    internal.and(j, ~mask);
  }

  def apply(i: Int):Boolean = {
    val j    = (i >>> 3);
    val mask = (1 << (i & 0x07));
    internal.get(j, mask);
  }

  def toByteArray: Array[Byte] = internal.freeze;
}
