/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

/** A module defining utility methods for higher-order functional programming.
 */
object Function {
  /** Given a sequence of functions `f,,1,,`, ..., `f,,n,,`, return the
   *  function `f,,1,, andThen ... andThen f,,n,,`.
   *
   *  @param fs The given sequence of functions
   */
  def chain[T](fs: scala.collection.Seq[T => T]): T => T = { x => fs.foldLeft(x)((x, f) => f(x)) }

  /** The constant function */
  def const[T, U](x: T)(y: U): T = x

  /** Turns a function `A => Option[B]` into a `PartialFunction[A, B]`.
   *
   *  '''Important note''': this transformation implies the original function
   *  may be called 2 or more times on each logical invocation, because the
   *  only way to supply an implementation of `isDefinedAt` is to call the
   *  function and examine the return value.
   *  See also [[scala.PartialFunction]], method `applyOrElse`.
   *
   *  @param   f    a function `T => Option[R]`
   *  @return       a partial function defined for those inputs where
   *                f returns `Some(_)` and undefined where `f` returns `None`.
   *  @see [[scala.PartialFunction]], method `lift`.
   */
  def unlift[T, R](f: T => Option[R]): PartialFunction[T, R] = PartialFunction.unlifted(f)

  /** Uncurrying for functions of arity 2. This transforms a unary function
   *  returning another unary function into a function of arity 2.
   */
  def uncurried[T1, T2, R](f: T1 => T2 => R): (T1, T2) => R = {
    (x1, x2) => f(x1)(x2)
  }

  /** Uncurrying for functions of arity 3.
   */
  def uncurried[T1, T2, T3, R](f: T1 => T2 => T3 => R): (T1, T2, T3) => R = {
    (x1, x2, x3) => f(x1)(x2)(x3)
  }

  /** Uncurrying for functions of arity 4.
   */
  def uncurried[T1, T2, T3, T4, R](f: T1 => T2 => T3 => T4 => R): (T1, T2, T3, T4) => R = {
    (x1, x2, x3, x4) => f(x1)(x2)(x3)(x4)
  }

  /** Uncurrying for functions of arity 5.
   */
  def uncurried[T1, T2, T3, T4, T5, R](f: T1 => T2 => T3 => T4 => T5 => R): (T1, T2, T3, T4, T5) => R  =  {
    (x1, x2, x3, x4, x5) => f(x1)(x2)(x3)(x4)(x5)
  }

  /** Tupling for functions of arity 2. This transforms a function
   *  of arity 2 into a unary function that takes a pair of arguments.
   *
   *  @note  These functions are slotted for deprecation, but it is on
   *  hold pending superior type inference for tupling anonymous functions.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, R](f: (T1, T2) => R): ((T1, T2)) => R = {
    case ((x1, x2)) => f(x1, x2)
  }

  /** Tupling for functions of arity 3. This transforms a function
   *  of arity 3 into a unary function that takes a triple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, T3, R](f: (T1, T2, T3) => R): ((T1, T2, T3)) => R = {
    case ((x1, x2, x3)) => f(x1, x2, x3)
  }

  /** Tupling for functions of arity 4. This transforms a function
   *  of arity 4 into a unary function that takes a 4-tuple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R): ((T1, T2, T3, T4)) => R = {
    case ((x1, x2, x3, x4)) => f(x1, x2, x3, x4)
  }

  /** Tupling for functions of arity 5. This transforms a function
   *  of arity 5 into a unary function that takes a 5-tuple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R): ((T1, T2, T3, T4, T5)) => R = {
    case ((x1, x2, x3, x4, x5)) => f(x1, x2, x3, x4, x5)
  }

  /** Un-tupling for functions of arity 2. This transforms a function taking
   *  a pair of arguments into a binary function which takes each argument separately.
   */
  def untupled[T1, T2, R](f: ((T1, T2)) => R): (T1, T2) => R = {
    (x1, x2) => f((x1, x2))
  }

  /** Un-tupling for functions of arity 3. This transforms a function taking
   *  a triple of arguments into a ternary function which takes each argument separately.
   */
  def untupled[T1, T2, T3, R](f: ((T1, T2, T3)) => R): (T1, T2, T3) => R = {
    (x1, x2, x3) => f((x1, x2, x3))
  }

  /** Un-tupling for functions of arity 4. This transforms a function taking
   *  a 4-tuple of arguments into a function of arity 4 which takes each argument separately.
   */
  def untupled[T1, T2, T3, T4, R](f: ((T1, T2, T3, T4)) => R): (T1, T2, T3, T4) => R = {
    (x1, x2, x3, x4) => f((x1, x2, x3, x4))
  }

  /** Un-tupling for functions of arity 5. This transforms a function taking
   *  a 5-tuple of arguments into a function of arity 5 which takes each argument separately.
   */
  def untupled[T1, T2, T3, T4, T5, R](f: ((T1, T2, T3, T4, T5)) => R): (T1, T2, T3, T4, T5) => R = {
    (x1, x2, x3, x4, x5) => f((x1, x2, x3, x4, x5))
  }
}
