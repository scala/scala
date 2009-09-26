/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

/** A trait for cloneable collections.
 *
 * @since 2.8
 */
@cloneable
trait Cloneable[+A <: AnyRef]  {
  override def clone: A = super.clone().asInstanceOf[A]
}
