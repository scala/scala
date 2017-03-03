/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala

/** A module defining utility methods for higher-order functional programming.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 29/11/2006
 */
object Function {
  /** Given a sequence of functions `f,,1,,`, ..., `f,,n,,`, return the
   *  function `f,,1,, andThen ... andThen f,,n,,`.
   *
   *  @param fs The given sequence of functions
   */
  def chain[a](fs: Seq[a => a]): a => a = { x => (x /: fs) ((x, f) => f(x)) }

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
  def uncurried[a1, a2, b](f: a1 => a2 => b): (a1, a2) => b = {
    (x1, x2) => f(x1)(x2)
  }

  /** Uncurrying for functions of arity 3.
   */
  def uncurried[a1, a2, a3, b](f: a1 => a2 => a3 => b): (a1, a2, a3) => b = {
    (x1, x2, x3) => f(x1)(x2)(x3)
  }

  /** Uncurrying for functions of arity 4.
   */
  def uncurried[a1, a2, a3, a4, b](f: a1 => a2 => a3 => a4 => b): (a1, a2, a3, a4) => b = {
    (x1, x2, x3, x4) => f(x1)(x2)(x3)(x4)
  }

  /** Uncurrying for functions of arity 5.
   */
  def uncurried[a1, a2, a3, a4, a5, b](f: a1 => a2 => a3 => a4 => a5 => b): (a1, a2, a3, a4, a5) => b  =  {
    (x1, x2, x3, x4, x5) => f(x1)(x2)(x3)(x4)(x5)
  }

  /** Tupling for functions of arity 2. This transforms a function
   *  of arity 2 into a unary function that takes a pair of arguments.
   *
   *  @note  These functions are slotted for deprecation, but it is on
   *  hold pending superior type inference for tupling anonymous functions.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[a1, a2, b](f: (a1, a2) => b): Tuple2[a1, a2] => b = {
    case Tuple2(x1, x2) => f(x1, x2)
  }

  /** Tupling for functions of arity 3. This transforms a function
   *  of arity 3 into a unary function that takes a triple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[a1, a2, a3, b](f: (a1, a2, a3) => b): Tuple3[a1, a2, a3] => b = {
    case Tuple3(x1, x2, x3) => f(x1, x2, x3)
  }

  /** Tupling for functions of arity 4. This transforms a function
   *  of arity 4 into a unary function that takes a 4-tuple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[a1, a2, a3, a4, b](f: (a1, a2, a3, a4) => b): Tuple4[a1, a2, a3, a4] => b = {
    case Tuple4(x1, x2, x3, x4) => f(x1, x2, x3, x4)
  }

  /** Tupling for functions of arity 5. This transforms a function
   *  of arity 5 into a unary function that takes a 5-tuple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[a1, a2, a3, a4, a5, b](f: (a1, a2, a3, a4, a5) => b): Tuple5[a1, a2, a3, a4, a5] => b = {
    case Tuple5(x1, x2, x3, x4, x5) => f(x1, x2, x3, x4, x5)
  }

  /** Un-tupling for functions of arity 2. This transforms a function taking
   *  a pair of arguments into a binary function which takes each argument separately.
   */
  def untupled[a1, a2, b](f: Tuple2[a1, a2] => b): (a1, a2) => b = {
    (x1, x2) => f(Tuple2(x1, x2))
  }

  /** Un-tupling for functions of arity 3. This transforms a function taking
   *  a triple of arguments into a ternary function which takes each argument separately.
   */
  def untupled[a1, a2, a3, b](f: Tuple3[a1, a2, a3] => b): (a1, a2, a3) => b = {
    (x1, x2, x3) => f(Tuple3(x1, x2, x3))
  }

  /** Un-tupling for functions of arity 4. This transforms a function taking
   *  a 4-tuple of arguments into a function of arity 4 which takes each argument separately.
   */
  def untupled[a1, a2, a3, a4, b](f: Tuple4[a1, a2, a3, a4] => b): (a1, a2, a3, a4) => b = {
    (x1, x2, x3, x4) => f(Tuple4(x1, x2, x3, x4))
  }

  /** Un-tupling for functions of arity 5. This transforms a function taking
   *  a 5-tuple of arguments into a function of arity 5 which takes each argument separately.
   */
  def untupled[a1, a2, a3, a4, a5, b](f: Tuple5[a1, a2, a3, a4, a5] => b): (a1, a2, a3, a4, a5) => b = {
    (x1, x2, x3, x4, x5) => f(Tuple5(x1, x2, x3, x4, x5))
  }
}
