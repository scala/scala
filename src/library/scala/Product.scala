/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** The trait <code>Product</code> defines access functions for instances
 *  of products, in particular case classes.
 *
 *  @author  Burak Emir
 *  @version 1.0
 *  @since   2.3
 */
trait Product extends Equals {

  /** for a product <code>A(x_1,...,x_k)</code>, returns <code>x_(n+1)</code>
   *  for <code>0 &lt;= n &lt; k</code>
   *
   *  @param  n the index of the element to return
   *  @throws IndexOutOfBoundsException
   *  @return  The element <code>n</code> elements after the first element
   */
  def productElement(n: Int): Any

  /** return k for a product <code>A(x_1,...,x_k)</code>
   */
  def productArity: Int

  /** An iterator that returns all fields of this product */
  def productIterator: Iterator[Any] = new Iterator[Any] {
    private var c: Int = 0
    private val cmax = productArity
    def hasNext = c < cmax
    def next() = { val result = productElement(c); c += 1; result }
  }

  @deprecated("use productIterator instead")
  def productElements: Iterator[Any] = productIterator

  /**
   *  By default the empty string. Implementations may override this
   *  method in order to prepend a string prefix to the result of the
   *  toString methods.
   */
  def productPrefix = ""

  /**
   *  An equality helper method to assist in maintaining reflexivity
   *  in the face of subtyping.  For more, see
   *    http://www.artima.com/lejava/articles/equality.html
   */
  override def canEqual(other: Any): Boolean = true
}
