/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2011, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

import scala.annotation.unchecked.uncheckedVariance

/** This class provides a default implementation of partial functions
 *  that is used for all partial function literals.
 *  It contains an optimized `orElse` method which supports
 *  chained `orElse` in linear time, and with no slow-down
 *  if the `orElse` part is not needed.
 *  The implementation of `orElse` works by cloning the abstract function object
 *  and modifying a private `fallBack` variable that encodes the `getorElse` part.
 */
abstract class AbstractPartialFunction[-T1, +R]
    extends AbstractFunction1[T1, R]
    with PartialFunction[T1, R]
    with Cloneable {

  private var fallBackField: PartialFunction[T1 @uncheckedVariance, R @uncheckedVariance] = _

  def fallBack: PartialFunction[T1, R] = synchronized {
    if (fallBackField == null) fallBackField = PartialFunction.empty
    fallBackField
  }

  override protected def missingCase(x: T1): R = fallBack(x)

  // Question: Need to ensure that fallBack is overwritten before any access
  // Is the `synchronized` here the right thing to achieve this?
  // Is there a cheaper way?
  override def orElse[A1 <: T1, B1 >: R](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] = {
    val result = this.clone.asInstanceOf[AbstractPartialFunction[A1, B1]]
    result.synchronized {
      result.fallBackField = this.fallBackField orElse that
      result
    }
  }

  def isDefinedAt(x: T1): scala.Boolean = _isDefinedAt(x) || fallBack.isDefinedAt(x)
  def _isDefinedAt(x: T1): scala.Boolean

}



