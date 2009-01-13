/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** A partial function of type <code>PartialFunction[A, B]</code> is a
 *  unary function where the domain does not include all values of type
 *  <code>A</code>. The function <code>isDefinedAt</code> allows to
 *  test dynamically, if a value is in the domain of the function.
 *
 *  @author  Martin Odersky
 *  @version 1.0, 16/07/2003
 */
trait ByNameFunction[-A, +B] extends AnyRef {
  def apply(x: => A): B
  override def toString() = "<function>"
}

