/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.ref

/**
 *  A wrapper class for java.lang.ref.WeakReference
 *  The new functionality is (1) results are Option values, instead of using null.
 *  (2) There is an extractor that maps the weak reference itself into an option.
 *  @author Sean McDirmid
 */
class WeakReference[+T <: AnyRef](value: T, queue: ReferenceQueue[T]) extends ReferenceWrapper[T] {
  def this(value: T) = this(value, null)
  val underlying: java.lang.ref.WeakReference[_ <: T] =
    new WeakReferenceWithWrapper[T](value, queue, this)
}

/** An extractor for weak reference values */
object WeakReference {

  /** Creates a weak reference pointing to `value` */
  def apply[T <: AnyRef](value: T) = new WeakReference(value)

  /** Optionally returns the referenced value, or `None` if that value no longer exists */
  def unapply[T <: AnyRef](wr: WeakReference[T]): Option[T] = Option(wr.underlying.get)
}

/**
 *  @author Philipp Haller
 */
private class WeakReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T], val wrapper: WeakReference[T])
  extends java.lang.ref.WeakReference[T](value, if (queue == null) null else queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
