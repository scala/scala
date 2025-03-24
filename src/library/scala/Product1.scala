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

object Product1 {
  def unapply[T1](x: Product1[T1]): Option[Product1[T1]] =
    Some(x)
}

/** Product1 is a Cartesian product of 1 component.
 */
trait Product1[@specialized(Int, Long, Double) +T1] extends Any with Product {
  /** The arity of this product.
   *  @return 1
   */
  override def productArity: Int = 1

  
  /** Returns the n-th projection of this product if 0 <= n < productArity,
   *  otherwise throws an `IndexOutOfBoundsException`.
   *
   *  @param n number of the projection to be returned
   *  @return  same as `._(n+1)`, for example `productElement(0)` is the same as `._1`.
   *  @throws  IndexOutOfBoundsException if the `n` is out of range(n < 0 || n >= 1).
   */

  @throws(classOf[IndexOutOfBoundsException])
  override def productElement(n: Int): Any = n match { 
    case 0 => _1
    case _ => throw new IndexOutOfBoundsException(s"$n is out of bounds (min 0, max 0)")
 }

  /** A projection of element 1 of this Product.
   *  @return   A projection of element 1.
   */
  def _1: T1


}
