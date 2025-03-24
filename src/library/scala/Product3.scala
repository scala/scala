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

object Product3 {
  def unapply[T1, T2, T3](x: Product3[T1, T2, T3]): Option[Product3[T1, T2, T3]] =
    Some(x)
}

/** Product3 is a Cartesian product of 3 components.
 */
trait Product3[+T1, +T2, +T3] extends Any with Product {
  /** The arity of this product.
   *  @return 3
   */
  override def productArity: Int = 3

  
  /** Returns the n-th projection of this product if 0 <= n < productArity,
   *  otherwise throws an `IndexOutOfBoundsException`.
   *
   *  @param n number of the projection to be returned
   *  @return  same as `._(n+1)`, for example `productElement(0)` is the same as `._1`.
   *  @throws  IndexOutOfBoundsException if the `n` is out of range(n < 0 || n >= 3).
   */

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int): Any = n match { 
    case 0 => _1
    case 1 => _2
    case 2 => _3
    case _ => throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max 2)")
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


}
