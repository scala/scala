/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

/** Base trait for all products, which in the standard library include at
 *  least [[scala.Product1]] through [[scala.Product22]] and therefore also
 *  their subclasses [[scala.Tuple1]] through [[scala.Tuple22]].  In addition,
 *  all case classes implement `Product` with synthetically generated methods.
 */
trait Product extends Any with Equals {
  /** The size of this product.
    *  @return     for a product `A(x,,1,,, ..., x,,k,,)`, returns `k`
    */
  def productArity: Int

  /** The n^th^ element of this product, 0-based.  In other words, for a
   *  product `A(x,,1,,, ..., x,,k,,)`, returns `x,,(n+1),,` where `0 <= n < k`.
   *
   *  @param    n   the index of the element to return
   *  @throws       IndexOutOfBoundsException if the `n` is out of range(n < 0 || n >= productArity).
   *  @return       the element `n` elements after the first element
   */
  def productElement(n: Int): Any

  /** An iterator over all the elements of this product.
   *  @return     in the default implementation, an `Iterator[Any]`
   */
  def productIterator: Iterator[Any] = new scala.collection.AbstractIterator[Any] {
    private[this] var c: Int = 0
    private[this] val cmax = productArity
    def hasNext: Boolean = c < cmax
    def next(): Any = { val result = productElement(c); c += 1; result }
  }

  /** A string used in the `toString` methods of derived classes.
   *  Implementations may override this method to prepend a string prefix
   *  to the result of `toString` methods.
   *
   *  @return   in the default implementation, the empty string
   */
  def productPrefix: String = ""

  /** The name of the n^th^ element of this product, 0-based.
   *  In the default implementation, an empty string.
   *
   *  @param    n   the index of the element name to return
   *  @throws       IndexOutOfBoundsException if the `n` is out of range(n < 0 || n >= productArity).
   *  @return       the name of the specified element
   */
  def productElementName(n: Int): String =
    if (n >= 0 && n < productArity) ""
    else throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max ${productArity-1})")

  /** An iterator over the names of all the elements of this product.
   */
  def productElementNames: Iterator[String] = new scala.collection.AbstractIterator[String] {
    private[this] var c: Int = 0
    private[this] val cmax = productArity
    def hasNext: Boolean = c < cmax
    def next(): String = { val result = productElementName(c); c += 1; result }
  }
}
