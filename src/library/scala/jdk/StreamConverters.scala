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

package scala.jdk

import scala.collection.convert.StreamExtensions

/** This object provides extension methods to create [[java.util.stream.Stream Java Streams]] that
  * operate on Scala collections (sequentially or in parallel). For more information on Java
  * streams, consult the documentation
  * ([[https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html]]).
  *
  * When writing Java code, use the explicit conversion methods defined in
  * [[javaapi.StreamConverters]] instead.
  *
  * The methods `asJavaSeqStream` and `asJavaParStream` convert a collection to a Java Stream:
  *
  * {{{
  *   scala> import scala.jdk.StreamConverters._
  *
  *   scala> val s = (1 to 10).toList.asJavaSeqStream
  *   s: java.util.stream.IntStream = java.util.stream.IntPipeline\$Head@7b1e5e55
  *
  *   scala> s.map(_ * 2).filter(_ > 5).toScala(List)
  *   res1: List[Int] = List(6, 8, 10, 12, 14, 16, 18, 20)
  * }}}
  *
  * Note: using parallel streams in the Scala REPL causes deadlocks, see
  * [[https://github.com/scala/bug/issues/9076]]. As a workaround, use `scala -Yrepl-class-based`.
  *
  * {{{
  *   scala> def isPrime(n: Int): Boolean = !(2 +: (3 to Math.sqrt(n).toInt by 2) exists (n % _ == 0))
  *   isPrime: (n: Int)Boolean
  *
  *   scala> (10000 to 1000000).asJavaParStream.filter(isPrime).toScala(Vector)
  *   res6: scala.collection.immutable.Vector[Int] = Vector(10007, 10009, 10037, 10039, ...
  * }}}
  *
  * A Java [[Stream]] provides operations on a sequence of elements. Streams are created from
  * [[java.util.Spliterator Spliterators]], which are similar to Iterators with the additional
  * capability to partition off some of their elements. This partitioning, if supported by the
  * Spliterator, is used for parallelizing Stream operations.
  *
  * Scala collections have a method [[scala.collection.IterableOnce.stepper `stepper`]] that
  * returns a [[scala.collection.Stepper]] for the collection, which in turn can be converted to a
  * Spliterator for creating a Java Stream.
  *
  * The `asJavaSeqStream ` extension method is available on any Scala collection. The
  * `asJavaParStream` extension method can only be invoked on collections where the return type of
  * the [[scala.collection.IterableOnce.stepper `stepper`]] method is marked with the
  * [[scala.collection.Stepper.EfficientSplit]] marker trait. This trait is added to steppers that
  * support partitioning, and therefore efficient parallel processing.
  *
  * The following extension methods are available:
  *
  * | Collection Type | Extension Methods |
  * | --- | --- |
  * | `IterableOnce` | `asJavaSeqStream` |
  * | `IndexedSeq`, Arrays, `BitSet`, `Accumulator`, `HashMap`, `HashSet`, `Range`, `TreeMap`, `TreeSet`, `Vector`, Strings | `asJavaParStream` |
  * | `Map` | `asJavaSeqKeyStream`, `asJavaSeqValueStream` |
  * | `HashMap`, `TreeMap` | `asJavaParKeyStream`, `asJavaParValueStream` |
  * | `Stepper` | `asJavaSeqStream` |
  * | `Stepper with EfficientSplit` | `asJavaParStream` |
  * | Strings | `asJavaSeqStream`, `asJavaParStream`, `asJavaSeqCharStream`, `asJavaParCharStream`, `asJavaSeqCodePointStream`, `asJavaParCodePointStream` |
  * | Java streams | `toScala`, `asJavaPrimitiveStream` |
  *
  * The `asJavaPrimitiveStream` method converts a `Stream[Int]` to an `IntStream`. It is the dual
  * of the `boxed` method defined on primitive streams (e.g., `IntStream.boxed` is a
  * `Stream[Integer]`).
  *
  * The `toScala` extension methods on Java streams collects the result of a stream pipeline into a
  * Scala collection, for example `stream.toScala(List)`, `stream.toScala(Vector)`. Note that
  * transformation operations on streams are lazy (also called "intermediate"), terminal operations
  * such as `forEach`, `count` or `toScala` trigger the evaluation.
  *
  * Collecting a parallel stream to a collection can be performed in parallel. This is beneficial if
  * the target collection supports efficient merging of the segments that are built in parallel.
  * To support this use case, the Scala standard library provides the [[Accumulator]] collection.
  * This collection supports efficient parallel construction, and it has specialized subtypes for
  * `Int`, `Long` and `Double` so that primitive Java streams can be collected to a Scala collection
  * without boxing the elements.
  */
object StreamConverters extends StreamExtensions
