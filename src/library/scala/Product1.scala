
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

object Product1 {
  def unapply[T1](x:Any): Option[Product1[T1]] =
    if(x.isInstanceOf[Product1[T1]]) Some(x.asInstanceOf[Product1[T1]]) else None
}

/** Product1 is a cartesian product of 1 components
 */
trait Product1 [+T1] extends Product {

  /**
   *  The arity of this product.
   *  @return 1
   */
  override def arity = 1

  /**
   *  Returns the n-th projection of this product if 0<=n<arity, otherwise null
   *  @param n number of the projection to be returned
   *  @return same as _(n+1)
   *  @throws IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {
    case 0 => _1
    case _ => throw new IndexOutOfBoundsException(n.toString())
  }

  /** projection of this product */
  def _1:T1


}
