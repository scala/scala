/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.ref

/**
 *  @author Sean McDirmid
 */
class ReferenceQueue[+T <: AnyRef] {
  private[ref] val underlying: java.lang.ref.ReferenceQueue[_ <: T] = new java.lang.ref.ReferenceQueue[T]
  override def toString = underlying.toString;
  class Wrapper[U <: AnyRef](val underlying: java.lang.ref.Reference[U]) extends ReferenceWrapper[U]
  def Wrapper[U <: AnyRef](ref: java.lang.ref.Reference[U]) = ref match {
    case null => None
    case ref => new Wrapper(ref)
  }
  def poll = Wrapper(underlying.poll)
  def remove = Wrapper(underlying.remove)
  def remove(timeout: Long) = Wrapper(underlying.remove(timeout))
}
