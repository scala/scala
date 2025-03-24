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

package scala
package runtime

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
 */
abstract class AbstractPartialFunction[@specialized(Specializable.Arg) -T1, @specialized(Specializable.Return) +R] extends Function1[T1, R] with PartialFunction[T1, R] { self =>
  // this method must be overridden for better performance,
  // for backwards compatibility, fall back to the one inherited from PartialFunction
  // this assumes the old-school partial functions override the apply method, though
  // override def applyOrElse[A1 <: T1, B1 >: R](x: A1, default: A1 => B1): B1 = ???

  // probably okay to make final since classes compiled before have overridden against the old version of AbstractPartialFunction
  // let's not make it final so as not to confuse anyone
  /*final*/ def apply(x: T1): R = applyOrElse(x, PartialFunction.empty)
}
