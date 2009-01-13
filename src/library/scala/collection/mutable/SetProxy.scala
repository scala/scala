/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable


/** This is a simple wrapper class for <a href="Set.html"
 *  target="contentFrame"><code>scala.collection.mutable.Set</code></a>.
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 09/05/2004
 */
trait SetProxy[A] extends Set[A] with collection.SetProxy[A] {

  def self: Set[A]

  override def update(elem: A, included: Boolean): Unit = self(elem) = included

  def +=(elem: A): Unit = self += elem

  override def ++=(that: Iterable[A]): Unit = self ++= that

  override def ++=(it: Iterator[A]): Unit = self ++= it

  override def incl(elems: A*): Unit = self ++= elems

  def -=(elem: A): Unit = self -= elem

  override def --=(that: Iterable[A]): Unit = self --= that

  override def --=(it: Iterator[A]): Unit = self --= it

  override def excl(elems: A*): Unit = self --= elems

  override def intersect(that: Set[A]): Unit = self.intersect(that)

  override def clear(): Unit = self.clear

  override def retain(p: A => Boolean): Unit =  self.retain(p)

  override def <<(cmd: Message[A]): Unit = self << cmd

  override def clone(): Set[A] = new SetProxy[A] {
    def self = SetProxy.this.self.clone()
  }
}
