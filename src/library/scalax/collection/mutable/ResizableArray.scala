/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: ResizableArray.scala 15407 2008-06-20 09:26:36Z stepancheg $


package scalax.collection.mutable

/** This class is used internally to implement data structures that
 *  are based on resizable arrays.
 *
 *  @author  Matthias Zenger, Burak Emir
 *  @version 1.0, 03/05/2004
 */
trait ResizableArray[A] extends Vector[A] {

  protected def initialSize: Int = 16
  protected var array: Array[AnyRef] = new Array[AnyRef](initialSize min 1)

  protected var size0: Int = 0

  //##########################################################################
  // implement/override methods of Vector[A]

  /** Returns the length of this resizable array.
   */
  def length: Int = size0

  def apply(idx: Int) = {
    if (idx >= size0) throw new IndexOutOfBoundsException(idx.toString)
    array(idx).asInstanceOf[A]
  }

  def update(idx: Int, elem: A) {
    if (idx >= size0) throw new IndexOutOfBoundsException(idx.toString)
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int) {
    Array.copy(array, 0, xs, start, size0)
  }

  /** Copy all elements to a buffer
   *  @param   The buffer to which elements are copied
   */
  override def copyToBuffer[B >: A](dest: Buffer[B]) {
    dest ++= array.asInstanceOf[Iterable[A]] // !!!
  }

  override def foreach(f: A => Unit) {
    var i = 0
    while (i < size) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  //##########################################################################

  /** remove elements of this array at indices after <code>sz</code>
   */
  def reduceToSize(sz: Int) {
    require(sz <= size0)
    size0 = sz
  }

  /** ensure that the internal array has at n cells */
  protected def ensureSize(n: Int) {
    if (n > array.length) {
      var newsize = array.length * 2
      while (n > newsize)
        newsize = newsize * 2
      val newar: Array[AnyRef] = new Array(newsize)
      Array.copy(array, 0, newar, 0, size0)
      array = newar
    }
  }

  /** Swap two elements of this array.
   */
  protected def swap(a: Int, b: Int) {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
  }

  /** Move parts of the array.
   */
  protected def copy(m: Int, n: Int, len: Int) {
    Array.copy(array, m, array, n, len)
  }
}
