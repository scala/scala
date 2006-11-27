
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

/** Product0 is a cartesian product of 0 components
 */
trait Product0  extends Product {

  /**
   *  The arity of this product.
   *  @return 0
   */
  override def arity = 0

  /**
   *  Returns the n-th projection of this product if 0<n<=arity, otherwise null
   *  @param n number of the projection to be returned
   *  @throws IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {
    case _ => throw new IndexOutOfBoundsException(n.toString())
  }


}
