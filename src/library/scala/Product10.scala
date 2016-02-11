/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

object Product10 {
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](x: Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]): Option[Product10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10]] =
    Some(x)
}

/** Product10 is a cartesian product of 10 components.
 *  @since 2.3
 */
trait Product10[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10] extends Any with Product {
  /** The arity of this product.
   *  @return 10
   */
  override def productArity = 10

  
  /** Returns the n-th projection of this product if 0 <= n < productArity,
   *  otherwise throws an `IndexOutOfBoundsException`.
   *
   *  @param n number of the projection to be returned
   *  @return  same as `._(n+1)`, for example `productElement(0)` is the same as `._1`.
   *  @throws  IndexOutOfBoundsException
   */

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int) = n match { 
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case 5 => _6
    case 6 => _7
    case 7 => _8
    case 8 => _9
    case 9 => _10
    case _ => throw new IndexOutOfBoundsException(n.toString())
 }

  /** A projection of element 1 of this Product.
   *  @return   A projection of element 1.
   */
  def _1: T1
  /** A projection of element 2 of this Product.
   *  @return   A projection of element 2.
   */
  def _2: T2
  /** A projection of element 3 of this Product.
   *  @return   A projection of element 3.
   */
  def _3: T3
  /** A projection of element 4 of this Product.
   *  @return   A projection of element 4.
   */
  def _4: T4
  /** A projection of element 5 of this Product.
   *  @return   A projection of element 5.
   */
  def _5: T5
  /** A projection of element 6 of this Product.
   *  @return   A projection of element 6.
   */
  def _6: T6
  /** A projection of element 7 of this Product.
   *  @return   A projection of element 7.
   */
  def _7: T7
  /** A projection of element 8 of this Product.
   *  @return   A projection of element 8.
   */
  def _8: T8
  /** A projection of element 9 of this Product.
   *  @return   A projection of element 9.
   */
  def _9: T9
  /** A projection of element 10 of this Product.
   *  @return   A projection of element 10.
   */
  def _10: T10


}
