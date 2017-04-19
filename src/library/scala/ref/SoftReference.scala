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
class SoftReference[+T <: AnyRef](value : T, queue : ReferenceQueue[T]) extends ReferenceWrapper[T] {
  def this(value : T) = this(value, null)

  val underlying: java.lang.ref.SoftReference[_ <: T] =
    new SoftReferenceWithWrapper[T](value, queue, this)
}

/**
 *  A companion object that implements an extractor for `SoftReference` values
 *  @author Rebecca Claire Murphy
 */
object SoftReference {

  /** Creates a `SoftReference` pointing to `value` */
  def apply[T <: AnyRef](value: T) = new SoftReference(value)

  /** Optionally returns the referenced value, or `None` if that value no longer exists */
  def unapply[T <: AnyRef](sr: SoftReference[T]): Option[T] = Option(sr.underlying.get)
}

/**
 *  @author Philipp Haller
 */
private class SoftReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T], val wrapper: SoftReference[T])
  extends java.lang.ref.SoftReference[T](value, if (queue == null) null else queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
