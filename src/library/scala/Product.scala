/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

/** Base trait for all products, which in the standard library include at
 *  least [[scala.Product1]] through [[scala.Product22]] and therefore also
 *  their subclasses [[scala.Tuple1]] through [[scala.Tuple22]].  In addition,
 *  all case classes implement `Product` with synthetically generated methods.
 *
 *  @author  Burak Emir
 *  @since   2.3
 */
trait Product extends Any with Equals {
  /** The n^th^ element of this product, 0-based.  In other words, for a
   *  product `A(x,,1,,, ..., x,,k,,)`, returns `x,,(n+1),,` where `0 <= n < k`.
   *
   *  @param    n   the index of the element to return
   *  @throws       IndexOutOfBoundsException
   *  @return       the element `n` elements after the first element
   */
  def productElement(n: Int): Any

  /** The size of this product.
   *  @return     for a product `A(x,,1,,, ..., x,,k,,)`, returns `k`
   */
  def productArity: Int

  /** An iterator over all the elements of this product.
   *  @return     in the default implementation, an `Iterator[Any]`
   */
  def productIterator: Iterator[Any] = new scala.collection.AbstractIterator[Any] {
    private[this] var c: Int = 0
    private[this] val cmax = productArity
    def hasNext = c < cmax
    def next() = { val result = productElement(c); c += 1; result }
  }

  /** A string used in the `toString` methods of derived classes.
   *  Implementations may override this method to prepend a string prefix
   *  to the result of `toString` methods.
   *
   *  @return   in the default implementation, the empty string
   */
  def productPrefix = ""

  def productElementName(n: Int): String =
    if (n >= 0 && n  < n) ""
    else throw new IndexOutOfBoundsException(n.toString)

  def productElementNames: Iterator[String] = new scala.collection.AbstractIterator[String] {
    private[this] var c: Int = 0
    private[this] val cmax = productArity
    def hasNext = c < cmax
    def next() = { val result = productElementName(c); c += 1; result }
  }
}
