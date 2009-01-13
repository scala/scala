/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Creates a buffer backed by a Java array list.
 *
 *  @author Sean McDirmid
 */
class ArrayList[A](override val underlying : java.util.ArrayList[A]) extends BufferWrapper[A]  {
  def this() = this(new java.util.ArrayList[A]);
  override def clone: ArrayList[A] =
    new ArrayList[A](underlying.clone().asInstanceOf[java.util.ArrayList[A]])
}
