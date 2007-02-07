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
  private[ref] val underlying = new java.lang.ref.ReferenceQueue
  override def toString = underlying.toString;
  class Wrapper(val underlying: java.lang.ref.Reference) extends ReferenceWrapper[T]
  def Wrapper(ref: java.lang.ref.Reference) = ref match {
    case null => None
    case ref => new Wrapper(ref)
  }
  def poll = Wrapper(underlying.poll)
  def remove = Wrapper(underlying.remove)
  def remove(timeout: Long) = Wrapper(underlying.remove(timeout))
}
