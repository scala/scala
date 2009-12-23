/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** A partial function of type `PartialFunction[A, B]` is a
 *  unary function where the domain does not necessarily include all values of type
 *  `A`. The function `isDefinedAt` allows to
 *  test dynamically if a value is in the domain of the function.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait PartialFunction[-A, +B] extends (A => B) {

  /** Checks if a value is contained in the function's domain.
   *
   *  @param  x   the value to test
   *  @return `true`, iff `x` is in the domain of this function, `false` otherwise.
   */
  def isDefinedAt(x: A): Boolean

  /** Composes this partial function with a fallback partial function which gets applied where this partial function
   *  is not defined.
   *
   *  @param   that    the fallback function
   *  @tparam  A1      the argument type of the fallback function
   *  @tparam  B1      the result type of the fallback function
   *  @return  a partial function which has as domain the union of the domains
   *           of this partial function and `that`. The resulting partial function
   *           takes `x` to `this(x)` where `this` is defined, and to `that(x)` where it is not.
   */
  def orElse[A1 <: A, B1 >: B](that: A1 =>? B1) : A1 =>? B1 =
    new PartialFunction[A1, B1] {
    def isDefinedAt(x: A1): Boolean =
      PartialFunction.this.isDefinedAt(x) || that.isDefinedAt(x)
    def apply(x: A1): B1 =
      if (PartialFunction.this.isDefinedAt(x)) PartialFunction.this.apply(x)
      else that.apply(x)
  }

  /**  Composes this partial function with a transformation function that gets applied
   *   to results of this partial function.
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a partial function with the same domain as this partial function, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C): A =>? C = new PartialFunction[A, C] {
    def isDefinedAt(x: A): Boolean = PartialFunction.this.isDefinedAt(x)
    def apply(x: A): C = k(PartialFunction.this.apply(x))
  }

  /** Turns this partial function into an plain function returning an `Option` result.
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = { x => if (isDefinedAt(x)) Some(this(x)) else None }
}

/** A few handy operations which leverage the extra bit of information
 *  available in partial functions.  Examples:
 *
 * <pre>
 *  import PartialFunction._
 *
 *  def strangeConditional(other: Any): Boolean = cond(other) {
 *    case x: String if x == "abc" || x == "def"  => true
 *    case x: Int => true
 *  }
 *  def onlyInt(v: Any): Option[Int] = condOpt(v) { case x: Int => x }
 * </pre>
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
object PartialFunction
{
  /** Creates a Boolean test based on a value and a partial function.
   *  It behaves like a 'match' statement with an implied 'case _ => false'
   *  following the supplied cases.
   *
   *  @param  x   the value to test
   *  @param  pf  the partial function
   *  @return true, iff `x` is in the domain of `pf` and `pf(x) == true`.
   */
  def cond[T](x: T)(pf: T =>? Boolean): Boolean =
    (pf isDefinedAt x) && pf(x)

  /** Transforms a PartialFunction[T, U] `pf' into Function1[T, Option[U]] `f'
   *  whose result is Some(x) if the argument is in pf's domain and None otherwise,
   *  and applies it to the value `x'.  In effect, it is a 'match' statement
   *  which wraps all case results in Some(_) and adds 'case _ => None' to the end.
   *
   *  @param  x     the value to test
   *  @param  pf    the PartialFunction[T, U]
   *  @return `Some(pf(x))` if `pf isDefinedAt x`, `None` otherwise.
   */
  def condOpt[T,U](x: T)(pf: T =>? U): Option[U] =
    if (pf isDefinedAt x) Some(pf(x)) else None
}
