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

class SoftReference[+T <: AnyRef](value : T, queue : ReferenceQueue[T]) extends ReferenceWrapper[T] {
  def this(value : T) = this(value, null)

  val underlying: java.lang.ref.SoftReference[_ <: T] =
    new SoftReferenceWithWrapper[T](value, queue, this)
}

/**
 *  A companion object that implements an extractor for `SoftReference` values
 */
object SoftReference {

  /** Creates a `SoftReference` pointing to `value` */
  def apply[T <: AnyRef](value: T): SoftReference[T] = new SoftReference(value)

  /** Optionally returns the referenced value, or `None` if that value no longer exists */
  def unapply[T <: AnyRef](sr: SoftReference[T]): Option[T] = Option(sr.underlying.get)
}

private class SoftReferenceWithWrapper[T <: AnyRef](value: T, queue: ReferenceQueue[T], val wrapper: SoftReference[T])
  extends java.lang.ref.SoftReference[T](value, if (queue == null) null else queue.underlying.asInstanceOf[java.lang.ref.ReferenceQueue[T]]) with ReferenceWithWrapper[T]
