/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala


/** Unit is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.  There is
 *  only one value of type Unit: `()`.
 */
final class Unit extends AnyVal { }


object Unit extends AnyValCompanion {

  /** Transform a value type into a boxed reference type.
   *
   *  @param  x   the Unit to be boxed
   *  @return     a scala.runtime.BoxedUnit offering `x` as its underlying value.
   */
  def box(x: Unit): scala.runtime.BoxedUnit = scala.runtime.BoxedUnit.UNIT

  /** Transform a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a scala.runtime.BoxedUnit.
   *
   *  @param  x   the scala.runtime.BoxedUnit to be unboxed.
   *  @throws     ClassCastException  if the argument is not a scala.runtime.BoxedUnit
   *  @return     the Unit value ()
   */
  def unbox(x: java.lang.Object): Unit = ()

  /** The String representation of the scala.Unit companion object.
   */
  override def toString = "object scala.Unit"
}

