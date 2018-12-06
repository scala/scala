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

package scala.collection.convert

import java.util.stream._

import scala.collection.AnyConstr
import scala.collection.convert.impl.{StepperShape, StreamShape, StreamUnboxer}

trait StreamConverters {
  // collections

  implicit class IterableHasSeqStream[A](cc: collection.IterableOps[A, AnyConstr, _]) {
    def seqStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S =
      s.fromStepper(cc.stepper, par = false)
  }

  implicit class IndexedSeqHasParStream[A](cc: collection.IndexedSeqOps[A, AnyConstr, _]) {
    def parStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S =
      s.fromStepper(cc.stepper, par = true)
  }

  // steppers

  implicit class StepperHasSeqStream[A](stepper: Stepper[A]) {
    def seqStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S =
      s.fromStepper(stepper.asInstanceOf[St], par = false)
  }

  implicit class StepperHasParStream[A](stepper: Stepper[A] with EfficientSubstep) {
    def parStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S =
      s.fromStepper(stepper.asInstanceOf[St], par = true)
  }

  // arrays
  // uses the JDK array spliterators (`DoubleArraySpliterator`). users can also call
  // `array.stepper.seqStream`, which then uses the Scala steppers (`DoubleArrayStepper`). the
  // steppers are also available on byte/short/char/float arrays (`WidenedByteArrayStepper`),
  // JDK spliterators only for double/int/long/reference.

  implicit class DoubleArrayHasSeqParStream(a: Array[Double]) {
    def seqStream: DoubleStream = java.util.Arrays.stream(a)
    def parStream: DoubleStream = seqStream.parallel
  }

  implicit class IntArrayHasSeqParStream(a: Array[Int]) {
    def seqStream: IntStream = java.util.Arrays.stream(a)
    def parStream: IntStream = seqStream.parallel
  }

  implicit class LongArrayHasSeqParStream(a: Array[Long]) {
    def seqStream: LongStream = java.util.Arrays.stream(a)
    def parStream: LongStream = seqStream.parallel
  }

  implicit class AnyArrayHasSeqParStream[A <: AnyRef](a: Array[A]) {
    def seqStream: Stream[A] = java.util.Arrays.stream(a)
    def parStream: Stream[A] = seqStream.parallel
  }

  // toScala for streams

  implicit class StreamHasToScala[A](stream: Stream[A]) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts streams of boxed integers, longs or
     * doubles are converted to the primitive accumulators ([[IntAccumulator]], etc.).
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[Accumulator]], which supports parallel building. The accumulator is
     * then converted to the target collection. Note that the stream is processed eagerly while
     * building the accumulator, even if the target collection is lazy.
     *
     * Sequential streams are directly converted to the target collection. If the target collection
     * is lazy, the conversion is lazy as well.
     */
    def toScala[C1](factory: collection.Factory[A, C1])(implicit info: AccumulatorFactoryInfo[A, C1]): C1 = {
      def anyAcc = stream.collect(AnyAccumulator.supplier[A], AnyAccumulator.adder[A], AnyAccumulator.merger[A])
      if (info.companion == AnyAccumulator) anyAcc.asInstanceOf[C1]
      else if (info.companion == IntAccumulator) stream.asInstanceOf[Stream[Int]].collect(IntAccumulator.supplier, IntAccumulator.boxedAdder, IntAccumulator.merger).asInstanceOf[C1]
      else if (info.companion == LongAccumulator) stream.asInstanceOf[Stream[Long]].collect(LongAccumulator.supplier, LongAccumulator.boxedAdder, LongAccumulator.merger).asInstanceOf[C1]
      else if (info.companion == DoubleAccumulator) stream.asInstanceOf[Stream[Double]].collect(DoubleAccumulator.supplier, DoubleAccumulator.boxedAdder, DoubleAccumulator.merger).asInstanceOf[C1]
      else if (stream.isParallel) anyAcc.to(factory)
      else factory.fromSpecific(collection.JavaConverters.asScalaIterator(stream.iterator))
    }

    def unboxed[S](implicit unboxer: StreamUnboxer[A, S]): S = unboxer(stream)
  }

  implicit class IntStreamHasToScala(stream: IntStream) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts the `IntStream` to a primitive
     * [[IntAccumulator]].
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[Accumulator]], which supports parallel building. The accumulator is
     * then converted to the target collection. Note that the stream is processed eagerly while
     * building the accumulator, even if the target collection is lazy.
     *
     * Sequential streams are directly converted to the target collection. If the target collection
     * is lazy, the conversion is lazy as well.
     */
    def toScala[C1](factory: collection.Factory[Int, C1])(implicit info: AccumulatorFactoryInfo[Int, C1]): C1 = {
      def intAcc = stream.collect(IntAccumulator.supplier, IntAccumulator.adder, IntAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Int], AnyAccumulator.unboxedIntAdder, AnyAccumulator.merger[Int]).asInstanceOf[C1]
      else if (info.companion == IntAccumulator) intAcc.asInstanceOf[C1]
      else if (stream.isParallel) intAcc.to(factory)
      else factory.fromSpecific(collection.JavaConverters.asScalaIterator(stream.iterator.asInstanceOf[java.util.Iterator[Int]]))
    }
  }

  implicit class LongStreamHasToScala(stream: LongStream) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts the `LongStream` to a primitive
     * [[LongAccumulator]].
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[Accumulator]], which supports parallel building. The accumulator is
     * then converted to the target collection. Note that the stream is processed eagerly while
     * building the accumulator, even if the target collection is lazy.
     *
     * Sequential streams are directly converted to the target collection. If the target collection
     * is lazy, the conversion is lazy as well.
     */
    def toScala[C1](factory: collection.Factory[Long, C1])(implicit info: AccumulatorFactoryInfo[Long, C1]): C1 = {
      def intAcc = stream.collect(LongAccumulator.supplier, LongAccumulator.adder, LongAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Long], AnyAccumulator.unboxedLongAdder, AnyAccumulator.merger[Long]).asInstanceOf[C1]
      else if (info.companion == LongAccumulator) intAcc.asInstanceOf[C1]
      else if (stream.isParallel) intAcc.to(factory)
      else factory.fromSpecific(collection.JavaConverters.asScalaIterator(stream.iterator.asInstanceOf[java.util.Iterator[Long]]))
    }
  }

  implicit class DoubleStreamHasToScala(stream: DoubleStream) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts the `DoubleStream` to a primitive
     * [[DoubleAccumulator]].
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[Accumulator]], which supports parallel building. The accumulator is
     * then converted to the target collection. Note that the stream is processed eagerly while
     * building the accumulator, even if the target collection is lazy.
     *
     * Sequential streams are directly converted to the target collection. If the target collection
     * is lazy, the conversion is lazy as well.
     */
    def toScala[C1](factory: collection.Factory[Double, C1])(implicit info: AccumulatorFactoryInfo[Double, C1]): C1 = {
      def intAcc = stream.collect(DoubleAccumulator.supplier, DoubleAccumulator.adder, DoubleAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Double], AnyAccumulator.unboxedDoubleAdder, AnyAccumulator.merger[Double]).asInstanceOf[C1]
      else if (info.companion == DoubleAccumulator) intAcc.asInstanceOf[C1]
      else if (stream.isParallel) intAcc.to(factory)
      else factory.fromSpecific(collection.JavaConverters.asScalaIterator(stream.iterator.asInstanceOf[java.util.Iterator[Double]]))
    }
  }
}
