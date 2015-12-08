/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala


/** A partial function of type `PartialFunction[A, B]` is a unary function
 *  where the domain does not necessarily include all values of type `A`.
 *  The function `isDefinedAt` allows to test dynamically if a value is in
 *  the domain of the function.
 *
 *  Even if `isDefinedAt` returns true for an `a: A`, calling `apply(a)` may
 *  still throw an exception, so the following code is legal:
 *
 *  {{{
 *  val f: PartialFunction[Int, Any] = { case _ => 1/0 }
 *  }}}
 *
 *  It is the responsibility of the caller to call `isDefinedAt` before
 *  calling `apply`, because if `isDefinedAt` is false, it is not guaranteed
 *  `apply` will throw an exception to indicate an error condition. If an
 *  exception is not thrown, evaluation may result in an arbitrary value.
 *
 *  The main distinction between `PartialFunction` and [[scala.Function1]] is
 *  that the user of a `PartialFunction` may choose to do something different
 *  with input that is declared to be outside its domain. For example:
 *
 *  {{{
 *  val sample = 1 to 10
 *  val isEven: PartialFunction[Int, String] = {
 *    case x if x % 2 == 0 => x+" is even"
 *  }
 *
 *  // the method collect can use isDefinedAt to select which members to collect
 *  val evenNumbers = sample collect isEven
 *
 *  val isOdd: PartialFunction[Int, String] = {
 *    case x if x % 2 == 1 => x+" is odd"
 *  }
 *
 *  // the method orElse allows chaining another partial function to handle
 *  // input outside the declared domain
 *  val numbers = sample map (isEven orElse isOdd)
 *  }}}
 *
 *
 *  @author  Martin Odersky, Pavel Pavlov, Adriaan Moors
 *  @version 1.0, 16/07/2003
 */
trait PartialFunction[-A, +B] extends (A => B) { self =>
  import PartialFunction._

  /** Checks if a value is contained in the function's domain.
   *
   *  @param  x   the value to test
   *  @return `'''true'''`, iff `x` is in the domain of this function, `'''false'''` otherwise.
   */
  def isDefinedAt(x: A): Boolean

  /** Composes this partial function with a fallback partial function which
   *  gets applied where this partial function is not defined.
   *
   *  @param   that    the fallback function
   *  @tparam  A1      the argument type of the fallback function
   *  @tparam  B1      the result type of the fallback function
   *  @return  a partial function which has as domain the union of the domains
   *           of this partial function and `that`. The resulting partial function
   *           takes `x` to `this(x)` where `this` is defined, and to `that(x)` where it is not.
   */
  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] =
    new OrElse[A1, B1] (this, that)
  //TODO: why not overload it with orElse(that: F1): F1?

  /**  Composes this partial function with a transformation function that
   *   gets applied to results of this partial function.
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a partial function with the same domain as this partial function, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C): PartialFunction[A, C] =
    new AndThen[A, B, C] (this, k)

  /** Turns this partial function into a plain function returning an `Option` result.
   *  @see     Function.unlift
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = new Lifted(this)

  /** Applies this partial function to the given argument when it is contained in the function domain.
   *  Applies fallback function where this partial function is not defined.
   *
   *  Note that expression `pf.applyOrElse(x, default)` is equivalent to
   *  {{{ if(pf isDefinedAt x) pf(x) else default(x) }}}
   *  except that `applyOrElse` method can be implemented more efficiently.
   *  For all partial function literals the compiler generates an `applyOrElse` implementation which
   *  avoids double evaluation of pattern matchers and guards.
   *  This makes `applyOrElse` the basis for the efficient implementation for many operations and scenarios, such as:
   *
   *  - combining partial functions into `orElse`/`andThen` chains does not lead to
   *    excessive `apply`/`isDefinedAt` evaluation
   *  - `lift` and `unlift` do not evaluate source functions twice on each invocation
   *  - `runWith` allows efficient imperative-style combining of partial functions
   *    with conditionally applied actions
   *
   *  For non-literal partial function classes with nontrivial `isDefinedAt` method
   *  it is recommended to override `applyOrElse` with custom implementation that avoids
   *  double `isDefinedAt` evaluation. This may result in better performance
   *  and more predictable behavior w.r.t. side effects.
   *
   *  @param  x       the function argument
   *  @param default  the fallback function
   *  @return   the result of this function or fallback function application.
   *  @since   2.10
   */
  def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
    if (isDefinedAt(x)) apply(x) else default(x)

  /** Composes this partial function with an action function which
   *  gets applied to results of this partial function.
   *  The action function is invoked only for its side effects; its result is ignored.
   *
   *  Note that expression `pf.runWith(action)(x)` is equivalent to
   *  {{{ if(pf isDefinedAt x) { action(pf(x)); true } else false }}}
   *  except that `runWith` is implemented via `applyOrElse` and thus potentially more efficient.
   *  Using `runWith` avoids double evaluation of pattern matchers and guards for partial function literals.
   *  @see `applyOrElse`.
   *
   *  @param   action  the action function
   *  @return  a function which maps arguments `x` to `isDefinedAt(x)`. The resulting function
   *           runs `action(this(x))` where `this` is defined.
   *  @since   2.10
   */
  def runWith[U](action: B => U): A => Boolean = { x =>
    val z = applyOrElse(x, checkFallback[B])
    if (!fallbackOccurred(z)) { action(z); true } else false
  }
}

/** A few handy operations which leverage the extra bit of information
 *  available in partial functions.  Examples:
 *  {{{
 *  import PartialFunction._
 *
 *  def strangeConditional(other: Any): Boolean = cond(other) {
 *    case x: String if x == "abc" || x == "def"  => true
 *    case x: Int => true
 *  }
 *  def onlyInt(v: Any): Option[Int] = condOpt(v) { case x: Int => x }
 *  }}}
 *
 *  @author  Paul Phillips
 *  @since   2.8
 */
object PartialFunction {
  /** Composite function produced by `PartialFunction#orElse` method
   */
  private class OrElse[-A, +B] (f1: PartialFunction[A, B], f2: PartialFunction[A, B])
    extends scala.runtime.AbstractPartialFunction[A, B] with Serializable {
    def isDefinedAt(x: A) = f1.isDefinedAt(x) || f2.isDefinedAt(x)

    override def apply(x: A): B = f1.applyOrElse(x, f2)

    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = {
      val z = f1.applyOrElse(x, checkFallback[B])
      if (!fallbackOccurred(z)) z else f2.applyOrElse(x, default)
    }

    override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) =
      new OrElse[A1, B1] (f1, f2 orElse that)

    override def andThen[C](k: B => C) =
      new OrElse[A, C] (f1 andThen k, f2 andThen k)
  }

  /** Composite function produced by `PartialFunction#andThen` method
   */
  private class AndThen[-A, B, +C] (pf: PartialFunction[A, B], k: B => C) extends PartialFunction[A, C] with Serializable {
    def isDefinedAt(x: A) = pf.isDefinedAt(x)

    def apply(x: A): C = k(pf(x))

    override def applyOrElse[A1 <: A, C1 >: C](x: A1, default: A1 => C1): C1 = {
      val z = pf.applyOrElse(x, checkFallback[B])
      if (!fallbackOccurred(z)) k(z) else default(x)
    }
  }

  /** To implement patterns like {{{ if(pf isDefinedAt x) f1(pf(x)) else f2(x) }}} efficiently
   *  the following trick is used:
   *
   *  To avoid double evaluation of pattern matchers & guards `applyOrElse` method is used here
   *  instead of `isDefinedAt`/`apply` pair.
   *
   *  After call to `applyOrElse` we need both the function result it returned and
   *  the fact if the function's argument was contained in its domain. The only degree of freedom we have here
   *  to achieve this goal is tweaking with the continuation argument (`default`) of `applyOrElse` method.
   *  The obvious way is to throw an exception from `default` function and to catch it after
   *  calling `applyOrElse` but I consider this somewhat inefficient.
   *
   *  I know only one way how you can do this task efficiently: `default` function should return unique marker object
   *  which never may be returned by any other (regular/partial) function. This way after calling `applyOrElse` you need
   *  just one reference comparison to distinguish if `pf isDefined x` or not.
   *
   *  This correctly interacts with specialization as return type of `applyOrElse`
   *  (which is parameterized upper bound) can never be specialized.
   *
   *  Here `fallback_pf` is used as both unique marker object and special fallback function that returns it.
   */
  private[this] val fallback_pf: PartialFunction[Any, Any] = { case _ => fallback_pf }
  private def checkFallback[B] = fallback_pf.asInstanceOf[PartialFunction[Any, B]]
  private def fallbackOccurred[B](x: B) = (fallback_pf eq x.asInstanceOf[AnyRef])

  private class Lifted[-A, +B] (val pf: PartialFunction[A, B])
      extends scala.runtime.AbstractFunction1[A, Option[B]] with Serializable {

    def apply(x: A): Option[B] = {
      val z = pf.applyOrElse(x, checkFallback[B])
      if (!fallbackOccurred(z)) Some(z) else None
    }
  }

  private class Unlifted[A, B] (f: A => Option[B]) extends scala.runtime.AbstractPartialFunction[A, B] with Serializable {
    def isDefinedAt(x: A): Boolean = f(x).isDefined

    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 = {
      val z = f(x)
      if (!z.isEmpty) z.get else default(x)
    }

    override def lift = f
  }

  private[scala] def unlifted[A, B](f: A => Option[B]): PartialFunction[A, B] = f match {
    case lf: Lifted[A, B] => lf.pf
    case ff => new Unlifted(ff)
  }

  /** Converts ordinary function to partial one
   *  @since   2.10
   */
  def apply[A, B](f: A => B): PartialFunction[A, B] = { case x => f(x) }

  private[this] val constFalse: Any => Boolean = { _ => false}

  private[this] val empty_pf: PartialFunction[Any, Nothing] = new PartialFunction[Any, Nothing] with Serializable {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) = throw new MatchError(x)
    override def orElse[A1, B1](that: PartialFunction[A1, B1]) = that
    override def andThen[C](k: Nothing => C) = this
    override val lift = (x: Any) => None
    override def runWith[U](action: Nothing => U) = constFalse
  }

  /** The partial function with empty domain.
   *  Any attempt to invoke empty partial function leads to throwing [[scala.MatchError]] exception.
   *  @since   2.10
   */
  def empty[A, B] : PartialFunction[A, B] = empty_pf

  /** Creates a Boolean test based on a value and a partial function.
   *  It behaves like a 'match' statement with an implied 'case _ => false'
   *  following the supplied cases.
   *
   *  @param  x   the value to test
   *  @param  pf  the partial function
   *  @return true, iff `x` is in the domain of `pf` and `pf(x) == true`.
   */
  def cond[T](x: T)(pf: PartialFunction[T, Boolean]): Boolean = pf.applyOrElse(x, constFalse)

  /** Transforms a PartialFunction[T, U] `pf` into Function1[T, Option[U]] `f`
   *  whose result is `Some(x)` if the argument is in `pf`'s domain and `None`
   *  otherwise, and applies it to the value `x`.  In effect, it is a
   *  `'''match'''` statement which wraps all case results in `Some(_)` and
   *  adds `'''case''' _ => None` to the end.
   *
   *  @param  x     the value to test
   *  @param  pf    the PartialFunction[T, U]
   *  @return `Some(pf(x))` if `pf isDefinedAt x`, `None` otherwise.
   */
  def condOpt[T,U](x: T)(pf: PartialFunction[T, U]): Option[U] = pf.lift(x)
}
