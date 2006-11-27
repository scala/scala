
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

/** Product6 is a cartesian product of 6 components
 */
trait Product6 [+T1, +T2, +T3, +T4, +T5, +T6] extends Product {

  /**
   *  The arity of this product.
   *  @return 6
   */
  override def arity = 6

  /**
   *  Returns the n-th projection of this product if 0<n<=arity, otherwise null
   *  @param n number of the projection to be returned
   *  @throws IndexOutOfBoundsException
   */
  override def element(n: Int) = n match {
    case 1 => _1
    case 2 => _2
    case 3 => _3
    case 4 => _4
    case 5 => _5
    case 6 => _6
    case _ => throw new IndexOutOfBoundsException(n.toString())
  }

  /** projection of this product */
  def _1:T1

  /** projection of this product */
  def _2:T2

  /** projection of this product */
  def _3:T3

  /** projection of this product */
  def _4:T4

  /** projection of this product */
  def _5:T5

  /** projection of this product */
  def _6:T6


}
