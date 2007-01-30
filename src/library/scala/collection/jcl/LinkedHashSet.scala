/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A set that is backed by a Java linked hash set, which fixes iteration
 *  order in terms of insertion order.
 *
 *  @author Sean McDirmid
 */
class LinkedHashSet[A](override val underlying: java.util.LinkedHashSet) extends CollectionWrapper[A] with SetWrapper[A] {
  def this() = this(new java.util.LinkedHashSet);
}
