/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.ref

import scala.collection.mutable.HashMap

/**
 *  @author Sean McDirmid, Philipp Haller
 */
class ReferenceQueue[+T <: AnyRef] {
  private[ref] val underlying: java.lang.ref.ReferenceQueue[_ <: T] = new java.lang.ref.ReferenceQueue[T]
  override def toString = underlying.toString

  protected def Wrapper(jref: java.lang.ref.Reference[_]) = jref match {
    case null => None
    case ref =>
      val refWrapper = wrappers(ref)
      wrappers -= ref
      Some(refWrapper.asInstanceOf[Reference[T]])
  }

  def poll: Option[Reference[T]] = Wrapper(underlying.poll)
  def remove: Option[Reference[T]] = Wrapper(underlying.remove)
  def remove(timeout: Long): Option[Reference[T]] = Wrapper(underlying.remove(timeout))

  protected val wrappers = new HashMap[java.lang.ref.Reference[_],
                                       ReferenceWrapper[_ <: AnyRef]]
  def register(ref: ReferenceWrapper[_ <: AnyRef]) {
    wrappers += ((ref.underlying, ref))
  }
}
