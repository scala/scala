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

package scala.collection


package object mutable {
  @deprecated("Use ArraySeq instead of WrappedArray; it can represent both, boxed and unboxed arrays", "2.13.0")
  type WrappedArray[X] = ArraySeq[X]
  @deprecated("Use ArraySeq instead of WrappedArray; it can represent both, boxed and unboxed arrays", "2.13.0")
  val WrappedArray = ArraySeq
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  type Traversable[X] = Iterable[X]
  @deprecated("Use Iterable instead of Traversable", "2.13.0")
  val Traversable = Iterable
  @deprecated("Use Stack instead of ArrayStack; it now uses an array-based implementation", "2.13.0")
  type ArrayStack[X] = Stack[X]
  @deprecated("Use Stack instead of ArrayStack; it now uses an array-based implementation", "2.13.0")
  val ArrayStack = Stack

  @deprecated("mutable.LinearSeq has been removed; use LinearSeq with mutable.Seq instead", "2.13.0")
  type LinearSeq[X] = Seq[X] with scala.collection.LinearSeq[X]

  @deprecated("GrowingBuilder has been renamed to GrowableBuilder", "2.13.0")
  type GrowingBuilder[Elem, To <: Growable[Elem]] = GrowableBuilder[Elem, To]

  @deprecated("IndexedOptimizedSeq has been renamed to IndexedSeq", "2.13.0")
  type IndexedOptimizedSeq[A] = IndexedSeq[A]

  @deprecated("IndexedOptimizedBuffer has been renamed to IndexedBuffer", "2.13.0")
  type IndexedOptimizedBuffer[A] = IndexedBuffer[A]
}
