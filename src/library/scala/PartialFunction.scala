/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

import annotation.experimental

/** A partial function of type <code>PartialFunction[A, B]</code> is a
 *  unary function where the domain does not include all values of type
 *  <code>A</code>. The function <code>isDefinedAt</code> allows to
 *  test dynamically, if a value is in the domain of the function.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait PartialFunction[-A, +B] extends AnyRef with (A => B) {

  /** Checks if a value is contained in the functions domain.
   *
   *  @param  x   the value to test
   *  @return true, iff <code>x</code> is in the domain of this function.
   */
  def isDefinedAt(x: A): Boolean

  def orElse[A1 <: A, B1 >: B](that: PartialFunction[A1, B1]) : PartialFunction[A1, B1] =
    new PartialFunction[A1, B1] {
    def isDefinedAt(x: A1): Boolean =
      PartialFunction.this.isDefinedAt(x) || that.isDefinedAt(x)
    def apply(x: A1): B1 =
      if (PartialFunction.this.isDefinedAt(x)) PartialFunction.this.apply(x)
      else that.apply(x)
  }

  override def andThen[C](k: B => C) : PartialFunction[A, C] = new PartialFunction[A, C] {
    def isDefinedAt(x: A): Boolean = PartialFunction.this.isDefinedAt(x)
    def apply(x: A): C = k(PartialFunction.this.apply(x))
  }
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
@experimental
object PartialFunction
{
  /** Creates a Boolean test based on a value and a partial function.
   *  It behaves like a 'match' statement with an implied 'case _ => false'
   *  following the supplied cases.
   *
   *  @param  x   the value to test
   *  @param  pf  the partial function
   *  @return true, iff <code>x</code> is in the domain of pf && pf(x) == true
   */
  def cond[T](x: T)(pf: PartialFunction[T, Boolean]): Boolean =
    (pf isDefinedAt x) && pf(x)

  /** Transforms a PartialFunction[T,U] `pf' into Function1[T,Option[U]] `f'
   *  whose result is Some(x) if the argument is in pf's domain and None otherwise,
   *  and applies it to the value `x'.  In effect, it is a 'match' statement
   *  which wraps all case results in Some(_) and adds 'case _ => None' to the end.
   *
   *  @param  x     the value to test
   *  @param  pf    the PartialFunction[T,U]
   *  @return Some(pf(x)) iff (pf isDefinedAt x) and None otherwise
   */
  def condOpt[T,U](x: T)(pf: PartialFunction[T, U]): Option[U] =
    if (pf isDefinedAt x) Some(pf(x)) else None

  // If only getOrElse were a bit less unwieldy...
  // def opt[T,U](x: T, default: U)(pf: PartialFunction[T, U]): U =
  //   opt(x)(pf) getOrElse default
}
