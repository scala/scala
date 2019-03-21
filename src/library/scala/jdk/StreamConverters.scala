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

package scala.jdk

import java.util.stream.{DoubleStream, IntStream, LongStream, Stream, StreamSupport}

import scala.collection.Stepper
import scala.collection.convert.StreamExtensions

/** This object contains methods to create [[java.util.stream.Stream Java Streams]] that operate on
  * Scala collections (sequentially or in parallel).
  *
  * Note: to convert between Scala collections and classic Java collections, use
  * [[CollectionConverters]].
  *
  * The explicit conversion methods defined here are practical when writing Java code. For Scala
  * code, it is recommended to use the extension methods defined in [[StreamConverters.Ops]].
  *
  * TODO: overview documentation
  *   - Stepeprs, Spliterators, EfficientSubstep
  *   - Available extension methods
  *   - Accumulators
  *   - Manual specializations
  *     - How to prevent boxing
  *     - Explain various shape classes
  *
  * @define parNote Note: parallel processing is only efficient for collections that have a
  *                 [[Stepper]] implementation which supports efficient splitting. For collections
  *                 where this is the case, the [[scala.collection.IterableOnce.stepper `stepper`]]
  *                 method has a return type marked `with EfficientSubstep`.
  */
object StreamConverters {

  /** This object provides extension methods to create [[java.util.stream.Stream Java Streams]] that
    * operate on Scala collections (sequentially or in parallel), see [[StreamConverters]].
    */
  object Ops extends StreamExtensions

  /////////////////////////////////////
  // sequential streams for collections
  /////////////////////////////////////

  /** Create a sequential [[java.util.stream.Stream Java Stream]] for a Scala collection. */
  def asJavaSeqStream[A](cc: IterableOnce[A]): Stream[A] = StreamSupport.stream(cc.stepper.spliterator, false)

  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection. */
  def asJavaSeqIntStream         (cc: IterableOnce[Int]):   IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection. */
  def asJavaSeqIntStreamFromByte (cc: IterableOnce[Byte]):  IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection. */
  def asJavaSeqIntStreamFromShort(cc: IterableOnce[Short]): IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for a Scala collection. */
  def asJavaSeqIntStreamFromChar (cc: IterableOnce[Char]):  IntStream = StreamSupport.intStream(cc.stepper.spliterator, false)

  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection. */
  def asJavaSeqDoubleStream         (cc: IterableOnce[Double]): DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, false)
  /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection. */
  def asJavaSeqDoubleStreamFromFloat(cc: IterableOnce[Float]):  DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, false)

  /** Create a sequential [[java.util.stream.LongStream Java LongStream]] for a Scala collection. */
  def asJavaSeqLongStream(cc: IterableOnce[Long]): LongStream = StreamSupport.longStream(cc.stepper.spliterator, false)

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
    */
  def asJavaParIntStream         (cc: IterableOnce[Int]):   IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $parNote
    */
  def asJavaParIntStreamFromByte (cc: IterableOnce[Byte]):  IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $parNote
    */
  def asJavaParIntStreamFromShort(cc: IterableOnce[Short]): IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for a Scala collection.
    *
    * $parNote
    */
  def asJavaParIntStreamFromChar (cc: IterableOnce[Char]):  IntStream = StreamSupport.intStream(cc.stepper.spliterator, true)

  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
    *
    * $parNote
    */
  def asJavaParDoubleStream         (cc: IterableOnce[Double]): DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, true)
  /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for a Scala collection.
    *
    * $parNote
    */
  def asJavaParDoubleStreamFromFloat(cc: IterableOnce[Float]):  DoubleStream = StreamSupport.doubleStream(cc.stepper.spliterator, true)

  /** Create a parallel [[java.util.stream.LongStream Java LongStream]] for a Scala collection.
    *
    * $parNote
    */
  def asJavaParLongStream(cc: IterableOnce[Long]): LongStream = StreamSupport.longStream(cc.stepper.spliterator, true)
}
