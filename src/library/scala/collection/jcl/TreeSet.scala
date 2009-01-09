/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl

/** Creates a sorted set that is backed by an underlying Java tree set.
 *  Elements of the sorted set are ordered with respect to the ordered
 *  view bound of <code>A</code>.
 *
 *  @author Sean McDirmid
 */
class TreeSet[A <% Ordered[A]] extends SortedSetWrapper[A] { ts =>
  val underlying = new java.util.TreeSet[A](new Comparator[A])
  override def clone: TreeSet[A] =
    new TreeSet[A] {
      override val underlying =
        ts.underlying.clone().asInstanceOf[java.util.TreeSet[A]]
    }
}
