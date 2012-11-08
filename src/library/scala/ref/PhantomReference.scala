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
class PhantomReference[+T <: AnyRef](value: T, queue: ReferenceQueue[T]) extends ReferenceWrapper[T] {
  val underlying: java.lang.ref.PhantomReference[_ <: T] =
    new PhantomReferenceWithWrapper[T](value, queue, this)
}

/**
 *  @author Philipp Haller
 */
private class PhantomReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T], val wrapper: PhantomReference[T])
  extends java.lang.ref.PhantomReference[T](value, queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
