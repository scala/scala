/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl

/** A set that is backed by a Java linked hash set, which fixes iteration
 *  order in terms of insertion order.
 *
 *  @author Sean McDirmid
 */
class LinkedHashSet[A](override val underlying: java.util.LinkedHashSet[A]) extends SetWrapper[A] {
  def this() = this(new java.util.LinkedHashSet[A])
  override def clone: LinkedHashSet[A] =
    new LinkedHashSet[A](underlying.clone().asInstanceOf[java.util.LinkedHashSet[A]])
}
