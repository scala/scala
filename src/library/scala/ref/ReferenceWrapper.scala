/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.ref

/**
 *  @author Sean McDirmid
 */
trait ReferenceWrapper[+T <: AnyRef] extends Reference[T] with Proxy {
  val underlying: java.lang.ref.Reference[_ <: T]
  override def get = Option(underlying.get)
  def apply() = {
    val ret = underlying.get
    if (ret eq null) throw new NoSuchElementException
    ret
  }
  def clear() = underlying.clear()
  def enqueue = underlying.enqueue
  def isEnqueued = underlying.isEnqueued
  def self = underlying
}

/**
 *  @author Philipp Haller
 */
private trait ReferenceWithWrapper[T <: AnyRef] {
  val wrapper: ReferenceWrapper[T]
}
