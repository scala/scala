/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
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
class ArrayList[A](override val underlying : java.util.ArrayList) extends CollectionWrapper[A] with BufferWrapper[A]  {
  def this() = this(new java.util.ArrayList);
  override def elements = super[BufferWrapper].elements;
}
