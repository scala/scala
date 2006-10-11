/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection


/** This is a simple wrapper class for <a href="Set.html"
 *  target="contentFrame"><code>scala.collection.Set</code></a>.
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 21/07/2003
 */
trait SetProxy[A] extends Set[A] with IterableProxy[A] {

  def self: Set[A]

  def size: Int = self.size

  override def isEmpty: Boolean = self.isEmpty

  def contains(elem: A): Boolean = self.contains(elem)

  override def subsetOf(that: Set[A]): Boolean = self.subsetOf(that)

  override def foreach(f: A => Unit): Unit = self.foreach(f)

  override def exists(p: A => Boolean): Boolean = self.exists(p)

}
