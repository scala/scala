/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
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
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait PartialFunction[-A, +B] extends (A => B) {

  /** Checks if a value is contained in the function's domain.
   *
   *  @param  x   the value to test
   *  @return `'''true'''`, iff `x` is in the domain of this function, `'''false'''` otherwise.
   */
  def isDefinedAt(x: A): Boolean

  //protected def missingCase[A1 <: A, B1 >: B]: PartialFunction[A1, B1] = PartialFunction.empty

  protected def missingCase(x: A): B = throw new MatchError(x)

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
  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] =
    new runtime.AbstractPartialFunction[A1, B1] {
      def _isDefinedAt(x: A1): Boolean =
        PartialFunction.this.isDefinedAt(x) || that.isDefinedAt(x)
      def apply(x: A1): B1 =
        if (PartialFunction.this.isDefinedAt(x)) PartialFunction.this.apply(x)
        else that.apply(x)
    }

  def orElseFast[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] =
    orElse(that)

  /**  Composes this partial function with a transformation function that
   *   gets applied to results of this partial function.
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a partial function with the same domain as this partial function, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C) : PartialFunction[A, C] = new runtime.AbstractPartialFunction[A, C] {
    def _isDefinedAt(x: A): Boolean = PartialFunction.this.isDefinedAt(x)
    def apply(x: A): C = k(PartialFunction.this.apply(x))
  }

  /** Turns this partial function into an plain function returning an `Option` result.
   *  @see     Function.unlift
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = new (A => Option[B]) {
    def apply(x: A): Option[B] = if (isDefinedAt(x)) Some(PartialFunction.this.apply(x)) else None
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
  private[this] final val empty_pf: PartialFunction[Any, Nothing] = new runtime.AbstractPartialFunction[Any, Nothing] {
    def _isDefinedAt(x: Any) = false
    override def isDefinedAt(x: Any) = false
    def apply(x: Any): Nothing = throw new MatchError(x)
    override def orElse[A1, B1](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = that
    override def orElseFast[A1, B1](that: PartialFunction[A1, B1]): PartialFunction[A1, B1] = that
    override def lift = (x: Any) => None
  }
  def empty[A, B] : PartialFunction[A, B] = empty_pf

  /** Creates a Boolean test based on a value and a partial function.
   *  It behaves like a 'match' statement with an implied 'case _ => false'
   *  following the supplied cases.
   *
   *  @param  x   the value to test
   *  @param  pf  the partial function
   *  @return true, iff `x` is in the domain of `pf` and `pf(x) == true`.
   */
  def cond[T](x: T)(pf: PartialFunction[T, Boolean]): Boolean =
    (pf isDefinedAt x) && pf(x)

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
  def condOpt[T,U](x: T)(pf: PartialFunction[T, U]): Option[U] =
    if (pf isDefinedAt x) Some(pf(x)) else None
}
