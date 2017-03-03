/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

object Product21 {
  def unapply[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](x: Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]): Option[Product21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21]] =
    Some(x)
}

/** Product21 is a Cartesian product of 21 components.
 *  @since 2.3
 */
trait Product21[+T1, +T2, +T3, +T4, +T5, +T6, +T7, +T8, +T9, +T10, +T11, +T12, +T13, +T14, +T15, +T16, +T17, +T18, +T19, +T20, +T21] extends Any with Product {
  /** The arity of this product.
   *  @return 21
   */
  override def productArity = 21

  
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
    case 10 => _11
    case 11 => _12
    case 12 => _13
    case 13 => _14
    case 14 => _15
    case 15 => _16
    case 16 => _17
    case 17 => _18
    case 18 => _19
    case 19 => _20
    case 20 => _21
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
  /** A projection of element 11 of this Product.
   *  @return   A projection of element 11.
   */
  def _11: T11
  /** A projection of element 12 of this Product.
   *  @return   A projection of element 12.
   */
  def _12: T12
  /** A projection of element 13 of this Product.
   *  @return   A projection of element 13.
   */
  def _13: T13
  /** A projection of element 14 of this Product.
   *  @return   A projection of element 14.
   */
  def _14: T14
  /** A projection of element 15 of this Product.
   *  @return   A projection of element 15.
   */
  def _15: T15
  /** A projection of element 16 of this Product.
   *  @return   A projection of element 16.
   */
  def _16: T16
  /** A projection of element 17 of this Product.
   *  @return   A projection of element 17.
   */
  def _17: T17
  /** A projection of element 18 of this Product.
   *  @return   A projection of element 18.
   */
  def _18: T18
  /** A projection of element 19 of this Product.
   *  @return   A projection of element 19.
   */
  def _19: T19
  /** A projection of element 20 of this Product.
   *  @return   A projection of element 20.
   */
  def _20: T20
  /** A projection of element 21 of this Product.
   *  @return   A projection of element 21.
   */
  def _21: T21


}
