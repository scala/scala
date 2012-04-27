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
  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] =
    new OrElse[A1, B1] (this, that)
  //TODO: why not overload it with orElse(that: F1): F1?

  /**  Composes this partial function with a transformation function that
   *   gets applied to results of this partial function.
   *   @param  k  the transformation function
   *   @tparam C  the result type of the transformation function.
   *   @return a partial function with the same domain as this partial function, which maps
   *           arguments `x` to `k(this(x))`.
   */
  override def andThen[C](k: B => C) : PartialFunction[A, C] = new PartialFunction[A, C] {
    def isDefinedAt(x: A): Boolean = self isDefinedAt x
    def apply(x: A): C = k(self(x))
  }

  /** Turns this partial function into an plain function returning an `Option` result.
   *  @see     Function.unlift
   *  @return  a function that takes an argument `x` to `Some(this(x))` if `this`
   *           is defined for `x`, and to `None` otherwise.
   */
  def lift: A => Option[B] = new Lifted(this)

  /**
   *  TODO: comment
   *  @since   2.10
   */
  def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
    if (isDefinedAt(x)) apply(x) else default(x)

  /**
   *  TODO: comment
   *  @since   2.10
   */
  def run[U](x: A)(action: B => U): Boolean =
    applyOrElse(x, fallbackToken) match {
      case FallbackToken => false
      case z => action(z); true
    }

  /**
   *  TODO: comment
   *  @since   2.10
   */
  def runWith[U](action: B => U): A => Boolean = { x => run(x)(action) }
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
  private final class OrElse[-A, +B] (f1: PartialFunction[A, B], f2: PartialFunction[A, B]) extends PartialFunction[A, B] {
    def isDefinedAt(x: A) = f1.isDefinedAt(x) || f2.isDefinedAt(x)

    def apply(x: A): B = f1.applyOrElse(x, f2)

    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
      f1.applyOrElse(x, fallbackToken) match {
        case FallbackToken => f2.applyOrElse(x, default)
        case z => z
      }

    override def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) =
      new OrElse[A1, B1] (f1, f2 orElse that)

    override def andThen[C](k: B => C) =
      new OrElse[A, C] (f1 andThen k, f2 andThen k)
  }

  private[scala] lazy val FallbackToken: PartialFunction[Any, PartialFunction[Any, Nothing]] = { case _ => FallbackToken.asInstanceOf[PartialFunction[Any, Nothing]] }
  private[scala] final def fallbackToken[B] = FallbackToken.asInstanceOf[PartialFunction[Any, B]]
  //TODO: check generated code for PF literal here

  private[scala] final class Lifted[-A, +B] (val pf: PartialFunction[A, B])
      extends runtime.AbstractFunction1[A, Option[B]] {

    def apply(x: A): Option[B] = pf.applyOrElse(x, fallbackToken) match {
      case FallbackToken => None
      case z => Some(z)
    }
  }

  private final class Unlifted[A, B] (f: A => Option[B]) extends runtime.AbstractPartialFunction[A, B] {
    def isDefinedAt(x: A): Boolean = f(x).isDefined
    override def applyOrElse[A1 <: A, B1 >: B](x: A1, default: A1 => B1): B1 =
      f(x) getOrElse default(x) //TODO: check generated code and inline getOrElse if needed
    override def lift = f
  }

  private[scala] def unlifted[A, B](f: A => Option[B]): PartialFunction[A, B] = f match {
    case lf: Lifted[A, B] => lf.pf
    case ff => new Unlifted(ff)
  }

  /** Converts ordinary function to partial one
   *  @since   2.10
   */
  //TODO: check generated code for PF literal here
  def apply[A, B](f: A => B): PartialFunction[A, B] = { case x => f(x) }

  private[this] final val constFalse: Any => Boolean = { _ => false}

  private[this] final val empty_pf: PartialFunction[Any, Nothing] = new PartialFunction[Any, Nothing] {
    def isDefinedAt(x: Any) = false
    def apply(x: Any) = throw new MatchError(x)
    override def orElse[A1, B1](that: PartialFunction[A1, B1]) = that
    override def andThen[C](k: Nothing => C) = this
    override val lift = (x: Any) => None
    override def run[U](x: Any)(action: Nothing => U) = false
    override def runWith[U](action: Nothing => U) = constFalse
  }

  /**
   *  TODO: comment
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
