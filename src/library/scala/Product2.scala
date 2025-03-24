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

object Product2 {
  def unapply[T1, T2](x: Product2[T1, T2]): Option[Product2[T1, T2]] =
    Some(x)
}

/** Product2 is a Cartesian product of 2 components.
 */
trait Product2[@specialized(Int, Long, Double) +T1, @specialized(Int, Long, Double) +T2] extends Any with Product {
  /** The arity of this product.
   *  @return 2
   */
  override def productArity: Int = 2

  
  /** Returns the n-th projection of this product if 0 <= n < productArity,
   *  otherwise throws an `IndexOutOfBoundsException`.
   *
   *  @param n number of the projection to be returned
   *  @return  same as `._(n+1)`, for example `productElement(0)` is the same as `._1`.
   *  @throws  IndexOutOfBoundsException if the `n` is out of range(n < 0 || n >= 2).
   */

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int): Any = n match { 
    case 0 => _1
    case 1 => _2
    case _ => throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max 1)")
 }

  /** A projection of element 1 of this Product.
   *  @return   A projection of element 1.
   */
  def _1: T1
  /** A projection of element 2 of this Product.
   *  @return   A projection of element 2.
   */
  def _2: T2


}
