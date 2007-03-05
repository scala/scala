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
trait Reference[+T <: AnyRef] extends Function0[T] {
  def isValid: Boolean
  def apply(): T
  def get = if (!isValid) None else Some(apply())
  override def toString = if (!isValid) "<deleted>" else apply().toString
  def clear(): Unit
  def enqueue(): Boolean
  def isEnqueued(): Boolean
}
