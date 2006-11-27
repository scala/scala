
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on Mon Nov 27 15:01:28 CET 2006
package scala

import Predef._

/** Product2 is a cartesian product of 2 components
 */
trait Product2 [+T1, +T2] extends Product {

  /**
   *  The arity of this product.
   *  @return 2
   */
  override def arity = 2

  /**
   *  Returns the n-th projection of this product if 0<n<=arity, otherwise null
   *  @param n number of the projection to be returned
   *  @throws IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {
    case 1 => _1
    case 2 => _2
    case _ => throw new IndexOutOfBoundsException(n.toString())
  }

  /** projection of this product */
  def _1:T1

  /** projection of this product */
  def _2:T2


}
