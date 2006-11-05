
/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// generated on Sun Nov 05 03:50:36 CET 2006
package scala

import Predef._

/** Product1 is a cartesian product of 1 components
 */
trait Product1 [+T1] extends Product {

  /**
   *  The arity of this product.
   *  @return 1
   */
  override def arity = 1

  /**
   *  Returns the n-th projection of this product if 0<n<=arity, otherwise null
   *  @param n number of the projection to be returned
   *  @throws IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {
    case 1 => _1
    case _ => throw new IndexOutOfBoundsException(n.toString())
  }

  /** projection of this product */
  def _1:T1


}
