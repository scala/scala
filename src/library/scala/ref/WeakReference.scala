/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.ref

/**
 *  A wrapper class for java.lang.ref.WeakReference
 *  The new functionality is (1) results are Option values, instead of using null.
 *  (2) There is an extractor that maps the weak reference itself into an option.
 */
class WeakReference[+T <: AnyRef](value: T, queue: ReferenceQueue[T]) extends ReferenceWrapper[T] {
  def this(value: T) = this(value, null)
  val underlying: java.lang.ref.WeakReference[_ <: T] =
    new WeakReferenceWithWrapper[T](value, queue, this)
}

/** An extractor for weak reference values */
object WeakReference {

  /** Creates a weak reference pointing to `value` */
  def apply[T <: AnyRef](value: T): WeakReference[T] = new WeakReference(value)

  /** Optionally returns the referenced value, or `None` if that value no longer exists */
  def unapply[T <: AnyRef](wr: WeakReference[T]): Option[T] = Option(wr.underlying.get)
}

private class WeakReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T], val wrapper: WeakReference[T])
  extends java.lang.ref.WeakReference[T](value, if (queue == null) null else queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
