/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

/** `Boolean` is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.
 *
 *  There is an implicit conversion from [[scala.Boolean]] => [[scala.runtime.RichBoolean]]
 *  which provides useful non-primitive operations.
 */
final class Boolean extends AnyVal {
  /**
   * Negates a Boolean expression.
   *
   * - `!a` results in `false` if and only if `a` evaluates to `true` and
   * - `!a` results in `true` if and only if `a` evaluates to `false`.
   *
   * @return the negated expression
   */
  def unary_! : Boolean = sys.error("stub")

  /**
    * Compares two Boolean expressions and returns `true` if they evaluate to the same value.
    *
    * `a == b` returns `true` if and only if
    *  - `a` and `b` are `true` or
    *  - `a` and `b` are `false`.
    */
  def ==(x: Boolean): Boolean = sys.error("stub")

  /**
    * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
    *
    * `a != b` returns `true` if and only if
    *  - `a` is `true` and `b` is `false` or
    *  - `a` is `false` and `b` is `true`.
    */
  def !=(x: Boolean): Boolean = sys.error("stub")

  /**
    * Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
    *
    * `a || b` returns `true` if and only if
    *  - `a` is `true` or
    *  - `b` is `true` or
    *  - `a` and `b` are `true`.
    *
    * @note This method uses 'short-circuit' evaluation and
    *       behaves as if it was declared as `def ||(x: => Boolean): Boolean`.
    *       If `a` evaluates to `true`, `true` is returned without evaluating `b`.
    */
  def ||(x: Boolean): Boolean = sys.error("stub")

  /**
    * Compares two Boolean expressions and returns `true` if both of them evaluate to true.
    *
    * `a && b` returns `true` if and only if
    *  - `a` and `b` are `true`.
    *
    * @note This method uses 'short-circuit' evaluation and
    *       behaves as if it was declared as `def &&(x: => Boolean): Boolean`.
    *       If `a` evaluates to `false`, `false` is returned without evaluating `b`.
    */
  def &&(x: Boolean): Boolean = sys.error("stub")

  // Compiler won't build with these seemingly more accurate signatures
  // def ||(x: => Boolean): Boolean = sys.error("stub")
  // def &&(x: => Boolean): Boolean = sys.error("stub")

  /**
    * Compares two Boolean expressions and returns `true` if one or both of them evaluate to true.
    *
    * `a | b` returns `true` if and only if
    *  - `a` is `true` or
    *  - `b` is `true` or
    *  - `a` and `b` are `true`.
    *
    * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
    */
  def |(x: Boolean): Boolean  = sys.error("stub")

  /**
    * Compares two Boolean expressions and returns `true` if both of them evaluate to true.
    *
    * `a & b` returns `true` if and only if
    *  - `a` and `b` are `true`.
    *
    * @note This method evaluates both `a` and `b`, even if the result is already determined after evaluating `a`.
    */
  def &(x: Boolean): Boolean  = sys.error("stub")

  /**
    * Compares two Boolean expressions and returns `true` if they evaluate to a different value.
    *
    * `a ^ b` returns `true` if and only if
    *  - `a` is `true` and `b` is `false` or
    *  - `a` is `false` and `b` is `true`.
    */
  def ^(x: Boolean): Boolean  = sys.error("stub")

  def getClass(): Class[Boolean] = sys.error("stub")
}

object Boolean extends AnyValCompanion {

  /** Transform a value type into a boxed reference type.
   *
   *  @param  x   the Boolean to be boxed
   *  @return     a java.lang.Boolean offering `x` as its underlying value.
   */
  def box(x: Boolean): java.lang.Boolean = java.lang.Boolean.valueOf(x)

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a java.lang.Boolean.
   *
   *  @param  x   the java.lang.Boolean to be unboxed.
   *  @throws     ClassCastException  if the argument is not a java.lang.Boolean
   *  @return     the Boolean resulting from calling booleanValue() on `x`
   */
  def unbox(x: java.lang.Object): Boolean = x.asInstanceOf[java.lang.Boolean].booleanValue()

  /** The String representation of the scala.Boolean companion object.
   */
  override def toString = "object scala.Boolean"
}

