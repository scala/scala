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

// GENERATED CODE: DO NOT EDIT. See scala.Function0 for timestamp.

package scala

object Product7 {
  def unapply[T1, T2, T3, T4, T5, T6, T7](x: Product7[T1, T2, T3, T4, T5, T6, T7]): Option[Product7[T1, T2, T3, T4, T5, T6, T7]] =
    Some(x)
}

/** Product7 is a Cartesian product of 7 components.
 */
trait Product7[+T1, +T2, +T3, +T4, +T5, +T6, +T7] extends Any with Product {
  /** The arity of this product.
   *  @return 7
   */
  override def productArity: Int = 7

  
  /** Returns the n-th projection of this product if 0 <= n < productArity,
   *  otherwise throws an `IndexOutOfBoundsException`.
   *
   *  @param n number of the projection to be returned
   *  @return  same as `._(n+1)`, for example `productElement(0)` is the same as `._1`.
   *  @throws  IndexOutOfBoundsException if the `n` is out of range(n < 0 || n >= 7).
   */

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int): Any = n match { 
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case 3 => _4
    case 4 => _5
    case 5 => _6
    case 6 => _7
    case _ => throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max 6)")
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


}
