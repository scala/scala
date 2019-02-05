/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
