/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.ref

/**
 *  @author Sean McDirmid
 */
class SoftReference[+T <: AnyRef](value : T, queue : ReferenceQueue[T]) extends ReferenceWrapper[T] {
  def this(value : T) = this(value, null);
  val underlying: java.lang.ref.SoftReference[_ <: T] =
    new SoftReferenceWithWrapper[T](value, queue, this)
}

/**
 *  @author Philipp Haller
 */
private class SoftReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T], val wrapper: SoftReference[T])
  extends java.lang.ref.SoftReference[T](value, if (queue == null) null else queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
