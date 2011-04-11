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
  def unary_! : Boolean = sys.error("stub")

  def ==(x: Boolean): Boolean = sys.error("stub")
  def !=(x: Boolean): Boolean = sys.error("stub")
  def ||(x: Boolean): Boolean = sys.error("stub")
  def &&(x: Boolean): Boolean = sys.error("stub")
  // Compiler won't build with these seemingly more accurate signatures
  // def ||(x: => Boolean): Boolean = sys.error("stub")
  // def &&(x: => Boolean): Boolean = sys.error("stub")
  def |(x: Boolean): Boolean  = sys.error("stub")
  def &(x: Boolean): Boolean  = sys.error("stub")
  def ^(x: Boolean): Boolean  = sys.error("stub")
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

