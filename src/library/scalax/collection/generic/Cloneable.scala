/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: CloneableCollection.scala 16893 2009-01-13 13:09:22Z cunei $


package scala.collection.generic

/** A trait for cloneable collections.
 */
trait Cloneable[A <: AnyRef] extends java.lang.Cloneable {
  override def clone(): A = super.clone().asInstanceOf[A]
}
