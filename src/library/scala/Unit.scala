/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// DO NOT EDIT, CHANGES WILL BE LOST.

package scala

import runtime.BoxedUnit

/** Unit is a member of the value classes, those whose instances are
 *  not represented as objects by the underlying host system.  There is
 *  only one value of type Unit: `()`.
 */
final class Unit extends AnyVal { }

object Unit extends AnyValCompanion {
  override def toString = "object scala.Unit"
  def box(x: Unit): BoxedUnit = BoxedUnit.UNIT
  def unbox(x: java.lang.Object): Unit = ()
}