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
 *  @author Seam McDirmid
 */
trait ReferenceWrapper[+T <: AnyRef] extends Reference[T] {
  val underlying: java.lang.ref.Reference
  def isValid = underlying.get != null
  def apply() = {
    val ret = underlying.get.asInstanceOf[T]
    if (ret eq null) throw new NoSuchElementException
    ret
  }
  def clear = underlying.clear
  def enqueue = underlying.enqueue
  def isEnqueued = underlying.isEnqueued
}
