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

package scala.jdk.javaapi

import java.util.stream.{DoubleStream, IntStream, LongStream, Stream, StreamSupport}
import java.{lang => jl}

/** This object contains methods to create Java Streams that operate on Scala collections
  * (sequentially or in parallel). For more information on Java streams, consult the documentation
  * ([[https://docs.oracle.com/javase/8/docs/api/java/util/stream/package-summary.html]]).
  *
  * The explicit conversion methods defined here are intended to be used in Java code. For Scala
  * code, it is recommended to use the extension methods defined in [[scala.jdk.StreamConverters]].
  *
  * Note: to convert between Scala collections and classic Java collections, use
  * [[CollectionConverters]].
  *
  * For details how the stream converters work, see [[scala.jdk.StreamConverters]].
  *
  * @define parNote Note: parallel processing is only efficient for collections that have a
  *                 [[scala.collection.Stepper]] implementation which supports efficient splitting. For collections
  *                 where this is the case, the [[scala.collection.IterableOnce.stepper `stepper`]]
  *                 method has a return type marked `with EfficientSplit`.
  *
  * @define primitiveNote Note: this method uses the boxed type `java.lang.X` instead of the
  *                       primitive type `scala.X` to improve compatibility when using it in
  *                       Java code (the Scala compiler emits `C[Int]` as `C[Object]` in bytecode
  *                       due to [[https://github.com/scala/bug/issues/4214 scala/bug#4214]]). In
  *                       Scala code, add `import scala.jdk.StreamConverters._` and use the
  *                       extension methods instead.
  */
object StreamConverters {
  /////////////////////////////////////
  // sequential streams for collections
  /////////////////////////////////////

  /** Create a sequential [[java.util.stream.Stream Java Stream]] for a Scala collection. */
  def asJavaSeqStream[A](cc: IterableOnce[A]): Stream[A] = StreamSupport.stream(cc.stepper.spliterator, false)

  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $primitiveNote
    */
  def asJavaSeqIntStream         (cc: IterableOnce[jl.Integer]):   IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $primitiveNote
    */
  def asJavaSeqIntStreamFromByte (cc: IterableOnce[jl.Byte]):      IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $primitiveNote
    */
  def asJavaSeqIntStreamFromShort(cc: IterableOnce[jl.Short]):     IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $primitiveNote
    */
  def asJavaSeqIntStreamFromChar (cc: IterableOnce[jl.Character]): IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)

  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
    *
    * $primitiveNote
    */
  def asJavaSeqDoubleStream         (cc: IterableOnce[jl.Double]): DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
    *
    * $primitiveNote
    */
  def asJavaSeqDoubleStreamFromFloat(cc: IterableOnce[jl.Float]):  DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, false)

  /** Create a sequential [[java.util.stream.LongStream Java LongStream]] for a Scala collection.
    *
    * $primitiveNote
    */
  def asJavaSeqLongStream(cc: IterableOnce[jl.Long]): LongStream = StreamSupport.longStream(cc.stepper.spliterator, false)

  // Map Key Streams

  /** Create a sequential [[java.util.stream.Stream Java Stream]] for the keys of a Scala Map. */
  def asJavaSeqKeyStream[K, V](m: collection.Map[K, V]): Stream[K] = StreamSupport.stream(m.keyStepper.spliterator, false)

  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $primitiveNote
    */
  def asJavaSeqKeyIntStream         [V](m: collection.Map[jl.Integer, V]):   IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $primitiveNote
    */
  def asJavaSeqKeyIntStreamFromByte [V](m: collection.Map[jl.Byte, V]):      IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $primitiveNote
    */
  def asJavaSeqKeyIntStreamFromShort[V](m: collection.Map[jl.Short, V]):     IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $primitiveNote
    */
  def asJavaSeqKeyIntStreamFromChar [V](m: collection.Map[jl.Character, V]): IntStream = StreamSupport.intStream(m.keyStepper.spliterator, false)

  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
    *
    * $primitiveNote
    */
  def asJavaSeqKeyDoubleStream         [V](m: collection.Map[jl.Double, V]): DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
    *
    * $primitiveNote
    */
  def asJavaSeqKeyDoubleStreamFromFloat[V](m: collection.Map[jl.Float, V]):  DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, false)

  /** Create a sequential [[java.util.stream.LongStream Java LongStream]] for the keys of a Scala Map.
    *
    * $primitiveNote
    */
  def asJavaSeqKeyLongStream[V](m: collection.Map[jl.Long, V]): LongStream = StreamSupport.longStream(m.keyStepper.spliterator, false)

  // Map Value Streams

  /** Create a sequential [[java.util.stream.Stream Java Stream]] for the values of a Scala Map. */
  def asJavaSeqValueStream[K, V](m: collection.Map[K, V]): Stream[V] = StreamSupport.stream(m.valueStepper.spliterator, false)

  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a
    *
    * $primitiveNote
    */
  def asJavaSeqValueIntStream         [K](m: collection.Map[K, jl.Integer]):   IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a
    *
    * $primitiveNote
    */
  def asJavaSeqValueIntStreamFromByte [K](m: collection.Map[K, jl.Byte]):      IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a
    *
    * $primitiveNote
    */
  def asJavaSeqValueIntStreamFromShort[K](m: collection.Map[K, jl.Short]):     IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for the values of a
    *
    * $primitiveNote
    */
  def asJavaSeqValueIntStreamFromChar [K](m: collection.Map[K, jl.Character]): IntStream = StreamSupport.intStream(m.valueStepper.spliterator, false)

  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a
    *
    * $primitiveNote
    */
  def asJavaSeqValueDoubleStream         [K](m: collection.Map[K, jl.Double]): DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, false)
  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a
    *
    * $primitiveNote
    */
  def asJavaSeqValueDoubleStreamFromFloat[K](m: collection.Map[K, jl.Float]):  DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, false)

  /** Create a sequential [[java.util.stream.LongStream Java LongStream]] for the values of a
    *
    * $primitiveNote
    */
  def asJavaSeqValueLongStream[K](m: collection.Map[K, jl.Long]): LongStream = StreamSupport.longStream(m.valueStepper.spliterator, false)

  ///////////////////////////////////
  // parallel streams for collections
  ///////////////////////////////////

  /** Create a parallel [[java.util.stream.Stream Java Stream]] for a Scala collection.
    *
    * $parNote
    */
  def asJavaParStream[A](cc: IterableOnce[A]): Stream[A] = StreamSupport.stream(cc.stepper.spliterator, true)

  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParIntStream         (cc: IterableOnce[jl.Integer]):   IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParIntStreamFromByte (cc: IterableOnce[jl.Byte]):      IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParIntStreamFromShort(cc: IterableOnce[jl.Short]):     IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParIntStreamFromChar (cc: IterableOnce[jl.Character]): IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)

  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParDoubleStream         (cc: IterableOnce[jl.Double]): DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParDoubleStreamFromFloat(cc: IterableOnce[jl.Float]):  DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, true)

  /** Create a parallel [[java.util.stream.LongStream Java LongStream]] for a Scala collection.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParLongStream(cc: IterableOnce[jl.Long]): LongStream = StreamSupport.longStream(cc.stepper.spliterator, true)


  // Map Key Streams

  /** Create a parallel [[java.util.stream.Stream Java Stream]] for the keys of a Scala Map.
    *
    * $parNote
    */
  def asJavaParKeyStream[K, V](m: collection.Map[K, V]): Stream[K] = StreamSupport.stream(m.keyStepper.spliterator, true)

  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParKeyIntStream         [V](m: collection.Map[jl.Integer, V]):   IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParKeyIntStreamFromByte [V](m: collection.Map[jl.Byte, V]):      IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParKeyIntStreamFromShort[V](m: collection.Map[jl.Short, V]):     IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the keys of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParKeyIntStreamFromChar [V](m: collection.Map[jl.Character, V]): IntStream = StreamSupport.intStream(m.keyStepper.spliterator, true)

  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParKeyDoubleStream         [V](m: collection.Map[jl.Double, V]): DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the keys of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParKeyDoubleStreamFromFloat[V](m: collection.Map[jl.Float, V]):  DoubleStream = StreamSupport.doubleStream(m.keyStepper.spliterator, true)

  /** Create a parallel [[java.util.stream.LongStream Java LongStream]] for the keys of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParKeyLongStream[V](m: collection.Map[jl.Long, V]): LongStream = StreamSupport.longStream(m.keyStepper.spliterator, true)

  // Map Value Streams

  /** Create a parallel [[java.util.stream.Stream Java Stream]] for the values of a Scala Map.
    *
    * $parNote
    */
  def asJavaParValueStream[K, V](m: collection.Map[K, V]): Stream[V] = StreamSupport.stream(m.valueStepper.spliterator, true)

  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParValueIntStream         [K](m: collection.Map[K, jl.Integer]):   IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParValueIntStreamFromByte [K](m: collection.Map[K, jl.Byte]):      IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParValueIntStreamFromShort[K](m: collection.Map[K, jl.Short]):     IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for the values of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParValueIntStreamFromChar [K](m: collection.Map[K, jl.Character]): IntStream = StreamSupport.intStream(m.valueStepper.spliterator, true)

  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParValueDoubleStream         [K](m: collection.Map[K, jl.Double]): DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, true)
  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for the values of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParValueDoubleStreamFromFloat[K](m: collection.Map[K, jl.Float]):  DoubleStream = StreamSupport.doubleStream(m.valueStepper.spliterator, true)

  /** Create a parallel [[java.util.stream.LongStream Java LongStream]] for the values of a Scala Map.
    *
    * $parNote
    *
    * $primitiveNote
    */
  def asJavaParValueLongStream[K](m: collection.Map[K, jl.Long]): LongStream = StreamSupport.longStream(m.valueStepper.spliterator, true)
}
