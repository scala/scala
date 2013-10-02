/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2013, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package runtime

import scala.annotation.unspecialized

/** `AbstractPartialFunction` reformulates all operations of its supertrait `PartialFunction`
 *  in terms of `isDefinedAt` and `applyOrElse`.
 *
 *  This allows more efficient implementations in many cases:
 *  - optimized `orElse` method supports chained `orElse` in linear time,
 *    and with no slow-down if the `orElse` part is not needed.
 *  - optimized `lift` method helps to avoid double evaluation of pattern matchers & guards
 *    of partial function literals.
 *
 *  This trait is used as a basis for implementation of all partial function literals.
 *
 *  @author  Pavel Pavlov
 *  @since   2.10
 */
abstract class AbstractPartialFunction[@specialized(scala.Int, scala.Long, scala.Float, scala.Double) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double) +R] extends Function1[T1, R] with PartialFunction[T1, R] { self =>
  // this method must be overridden for better performance,
  // for backwards compatibility, fall back to the one inherited from PartialFunction
  // this assumes the old-school partial functions override the apply method, though
  // override def applyOrElse[A1 <: T1, B1 >: R](x: A1, default: A1 => B1): B1 = ???

  // probably okay to make final since classes compiled before have overridden against the old version of AbstractPartialFunction
  // let's not make it final so as not to confuse anyone
  /*final*/ def apply(x: T1): R = applyOrElse(x, PartialFunction.empty)
}

// Manual stand-ins for formerly specialized variations.
// Not comprehensive, only sufficent to run scala-check built scala 2.11.0-M5
// TODO Scala 2.10.0.M6 Remove this once scalacheck is published against M6.
private[runtime] abstract class AbstractPartialFunction$mcIL$sp extends scala.runtime.AbstractPartialFunction[Any, Int] {
    override def apply(x: Any): Int = apply$mcIL$sp(x)
    def apply$mcIL$sp(x: Any): Int = applyOrElse(x, PartialFunction.empty)
}
private[runtime] abstract class AbstractPartialFunction$mcFL$sp extends scala.runtime.AbstractPartialFunction[Any, Float] {
    override def apply(x: Any): Float = apply$mcIL$sp(x)
    def apply$mcIL$sp(x: Any): Float = applyOrElse(x, PartialFunction.empty)
}
