/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class is used internally to implement data structures that
 *  are based on resizable arrays.
 *  //todo enrich with more efficient operations
 *
 *  @author  Matthias Zenger, Burak Emir
 *  @version 1.0, 03/05/2004
 */
trait ResizableArray[A] extends Seq[A] {

  protected val initialSize: Int = 16
  protected var array: Array[A] = new Array[A](initialSize)
  protected var size: Int = 0

  //##########################################################################
  // implement/override methods of Seq[A]

  /** Returns the length of this resizable array.
   */
  def length: Int = size

  def apply(i: Int) = array(i)

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int): Unit =
    Array.copy(array, 0, xs, start, size)

  /** Copy all elements to a buffer
   *  @param   The buffer to which elements are copied
   */
  override def copyToBuffer[B >: A](dest: Buffer[B]): Unit =
    dest.++=(array.asInstanceOf[Array[B]], 0, size)

  /** Returns a new iterator over all elements of this resizable array.
   */
  def elements: Iterator[A] = new Iterator[A] {
    var i = 0
    def hasNext: Boolean = i < size
    def next(): A = { i = i + 1; array(i - 1) }
  }

  //##########################################################################

  /** ensure that the internal array has at n cells */
  protected def ensureSize(n: Int): Unit =
    if (n > array.length) {
      var newsize = array.length * 2
      while (n > newsize)
        newsize = newsize * 2
      val newar: Array[A] = new Array(newsize)
      Array.copy(array, 0, newar, 0, size)
      array = newar
    }

  /** Swap two elements of this array.
   */
  protected def swap(a: Int, b: Int): Unit = {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
  }

  /** Move parts of the array.
   */
  protected def copy(m: Int, n: Int, len: Int) =
    Array.copy(array, m, array, n, len)

}
