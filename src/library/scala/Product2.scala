
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on Tue Nov 28 17:25:56 CET 2006
package scala

import Predef._

object Product2 {
  def unapply[T1, T2](x:Any): Option[Product2[T1, T2]] =
    if(x.isInstanceOf[Product2[T1, T2]]) Some(x.asInstanceOf[Product2[T1, T2]]) else None
}

/** Product2 is a cartesian product of 2 components
 */
trait Product2 [+T1, +T2] extends Product {

  /**
   *  The arity of this product.
   *  @return 2
   */
  override def arity = 2

  /**
   *  Returns the n-th projection of this product if 0<=n<arity, otherwise null
   *  @param n number of the projection to be returned
   *  @return same as _(n+1)
   *  @throws IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {
    case 0 => _1
    case 1 => _2
    case _ => throw new IndexOutOfBoundsException(n.toString())
  }

  /** projection of this product */
  def _1:T1

  /** projection of this product */
  def _2:T2


}
