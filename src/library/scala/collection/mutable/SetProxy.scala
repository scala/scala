/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.mutable

import generic.SetProxyTemplate

/** This is a simple wrapper class for <a href="Set.html"
 *  target="contentFrame"><code>scala.collection.mutable.Set</code></a>.
 *  It is most useful for assembling customized set abstractions
 *  dynamically using object composition and forwarding.
 *
 *  @author  Matthias Zenger
 *  @version 1.1, 09/05/2004
 */
trait SetProxy[A] extends mutable.Set[A] with SetProxyTemplate[A, mutable.Set[A]]
{
  override def thisCollection = this
  override def empty = new SetProxy[A] { val self = SetProxy.this.self.empty }
  override def + (elem: A) = { self += elem ; this }
  override def - (elem: A) = { self -= elem ; this }

  def +=(elem: A) = { self += elem; this }
  def -=(elem: A) = { self -= elem; this }
}