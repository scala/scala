/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** A hash set that is backed by a Java hash set.
 *
 *  @author Sean McDirmid
 */
class HashSet[A](override val underlying: java.util.HashSet[A]) extends SetWrapper[A] {
  /** Creates an underlying Java hash set. */
  def this() = this(new java.util.HashSet[A]);

  override def clone: HashSet[A] =
    new HashSet[A](underlying.clone().asInstanceOf[java.util.HashSet[A]])
}
