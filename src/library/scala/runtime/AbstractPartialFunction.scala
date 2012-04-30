/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2011, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

/** `AbstractPartialFunction` reformulates all operations of its supertrait `PartialFunction` in terms of `isDefinedAt` and `applyOrElse`.
 *
 *  This allows more efficient implementations in many cases:
 *  - optimized `orElse` method supports chained `orElse` in linear time,
 *    and with no slow-down if the `orElse` part is not needed.
 *  - optimized `lift` method helps to avoid double evaluation of pattern matchers & guards
 *    of partial function literals.
 *
 *  This trait is used as a basis for implementation of all partial function literals
 *  with non-exhaustive matchers.
 *
 *  Use of `AbstractPartialFunction` instead of `PartialFunction` as a base trait for
 *  user-defined partial functions may result in better performance
 *  and more predictable behavior w.r.t. side effects.
 *
 *  @author  Pavel Pavlov
 *  @since   2.10
 */
abstract class AbstractPartialFunction[@specialized(scala.Int, scala.Long, scala.Float, scala.Double, scala.AnyRef) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double, scala.AnyRef) +R] extends Function1[T1, R] with PartialFunction[T1, R] { self =>
  // this method must be overridden for better performance,
  // for backwards compatibility, fall back to the one inherited from PartialFunction
  // this assumes the old-school partial functions override the apply method, though
  // override def applyOrElse[A1 <: T1, B1 >: R](x: A1, default: A1 => B1): B1 = ???

  // probably okay to make final since classes compiled before have overridden against the old version of AbstractPartialFunction
  // let's not make it final so as not to confuse anyone
  /*final*/ def apply(x: T1): R = applyOrElse(x, PartialFunction.empty)

  @annotation.unspecialized override final def andThen[C](k: R => C) : PartialFunction[T1, C] =
    new AbstractPartialFunction[T1, C] {
      def isDefinedAt(x: T1): Boolean = self.isDefinedAt(x)
      override def applyOrElse[A1 <: T1, C1 >: C](x: A1, default: A1 => C1): C1 =
        self.applyOrElse(x, PartialFunction.fallbackToken) match {
          case PartialFunction.FallbackToken => default(x)
          case z => k(z)
        }
    }

  // TODO: remove
  protected def missingCase(x: T1): R = throw new MatchError(x)
}


/** `AbstractTotalFunction` is a partial function whose `isDefinedAt` method always returns `true`.
 *
 * This class is used as base class for partial function literals with
 * certainly exhaustive pattern matchers.
 *
 *  @author  Pavel Pavlov
 *  @since   2.10
 */
abstract class AbstractTotalFunction[@specialized(scala.Int, scala.Long, scala.Float, scala.Double, scala.AnyRef) -T1, @specialized(scala.Unit, scala.Boolean, scala.Int, scala.Float, scala.Long, scala.Double, scala.AnyRef) +R] extends Function1[T1, R] with PartialFunction[T1, R] {
  final def isDefinedAt(x: T1): Boolean = true
  @annotation.unspecialized override final def applyOrElse[A1 <: T1, B1 >: R](x: A1, default: A1 => B1): B1 = apply(x)
  @annotation.unspecialized override final def orElse[A1 <: T1, B1 >: R](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = this
  //TODO: check generated code for PF literal here
  @annotation.unspecialized override final def andThen[C](k: R => C): PartialFunction[T1, C] = { case x => k(apply(x)) }
}
