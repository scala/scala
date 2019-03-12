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

import java.util.Spliterator
import java.util.stream._

import scala.annotation.implicitNotFound
import scala.collection.AnyConstr
import scala.collection.convert.impl.{AccumulatorFactoryInfo, StepperShape, StreamShape, StreamUnboxer}

trait StreamConverters {
  // collections

  implicit class IterableHasSeqStream[A](cc: IterableOnce[A]) {
    def seqStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S =
      s.fromStepper(cc.stepper, par = false)
  }

  protected type IterableOnceWithEfficientStepper[A] = IterableOnce[A] {
    def stepper[B >: A, S <: Stepper[_]](implicit shape : StepperShape[B, S]) : S with EfficientSubstep
  }

  // Not `CC[X] <: IterableOnce[X]`, but `C` with an extra constraint, to support non-parametric classes like IntAccumulator
  implicit class IterableNonGenericHasParStream[A, C <: IterableOnce[_]](c: C)(implicit ev: C <:< IterableOnce[A]) {
    def parStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit
        s: StreamShape[A, S, St],
        st: StepperShape[A, St],
        @implicitNotFound("`parStream` can only be called on collections where `stepper` returns a `Stepper with EfficientSubstep`")
        isEfficient: C <:< IterableOnceWithEfficientStepper[A]): S =
      s.fromStepper(ev(c).stepper, par = true)
  }

  // maps

  implicit class MapHasSeqKeyValueStream[K, V](cc: collection.MapOps[K, V, AnyConstr, _]) {
    def seqKeyStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit s: StreamShape[K, S, St], st: StepperShape[K, St]): S =
      s.fromStepper(cc.keyStepper, par = false)

    def seqValueStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit s: StreamShape[V, S, St], st: StepperShape[V, St]): S =
      s.fromStepper(cc.valueStepper, par = false)
  }

  private type MapOpsWithEfficientKeyStepper[K, V] = collection.MapOps[K, V, AnyConstr, _] { def keyStepper[S <: Stepper[_]](implicit shape : StepperShape[K, S]) : S with EfficientSubstep }
  private type MapOpsWithEfficientValueStepper[K, V] = collection.MapOps[K, V, AnyConstr, _] { def valueStepper[V1 >: V, S <: Stepper[_]](implicit shape : StepperShape[V1, S]) : S with EfficientSubstep }
  private type MapOpsWithEfficientStepper[K, V] = collection.MapOps[K, V, AnyConstr, _] { def stepper[B >: (K, V), S <: Stepper[_]](implicit shape : StepperShape[B, S]) : S with EfficientSubstep }

  implicit class MapHasParKeyValueStream[K, V, CC[X, Y] <: collection.MapOps[X, Y, AnyConstr, _]](cc: CC[K, V]) {
    def parKeyStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit
        s: StreamShape[K, S, St],
        st: StepperShape[K, St],
        @implicitNotFound("parKeyStream can only be called on maps where `keyStepper` returns a `Stepper with EfficientSubstep`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientKeyStepper[K, V]): S =
      s.fromStepper(cc.keyStepper, par = true)

    def parValueStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit
        s: StreamShape[V, S, St],
        st: StepperShape[V, St],
        @implicitNotFound("parValueStream can only be called on maps where `valueStepper` returns a `Stepper with EfficientSubstep`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientValueStepper[K, V]): S =
      s.fromStepper(cc.valueStepper, par = true)

    // The parStream extension method for IterableOnce doesn't apply because its `CC` takes a single type parameter, whereas the one here takes two
    def parStream[S <: BaseStream[_, S], St <: Stepper[_]](implicit
        s: StreamShape[(K, V), S, St],
        st: StepperShape[(K, V), St],
        @implicitNotFound("parStream can only be called on maps where `stepper` returns a `Stepper with EfficientSubstep`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientStepper[K, V]): S =
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

  // strings

  implicit class StringHasSeqParStream(s: String) {
    /**
     * A sequential stream on the characters of a string, same as [[seqCharStream]]. See also
     * [[seqCodePointStream]].
     */
    def seqStream: IntStream = StreamSupport.intStream(s.stepper, /* par = */ false)
    /**
     * A parallel stream on the characters of a string, same as [[parCharStream]]. See also
     * [[parCodePointStream]].
     */
    def parStream: IntStream = StreamSupport.intStream(s.stepper, /* par = */ true)

    /** A sequential stream on the characters of a string. See also  [[seqCodePointStream]]. */
    def seqCharStream: IntStream = StreamSupport.intStream(s.charStepper, /* par = */ false)
    /** A parallel stream on the characters of a string. See also [[parCodePointStream]]. */
    def parCharStream: IntStream = StreamSupport.intStream(s.charStepper, /* par = */ true)

    /** A sequential stream on the code points of a string. See also [[seqCharStream]]. */
    def seqCodePointStream: IntStream = StreamSupport.intStream(s.codePointStepper, /* par = */ false)
    /** A parallel stream on the code points of a string. See also [[parCharStream]]. */
    def parCodePointStream: IntStream = StreamSupport.intStream(s.codePointStepper, /* par = */ true)
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

  implicit class SpliteratorHasStepper[A](s: Spliterator[A]) {
    def stepper: Stepper[A] = Stepper.ofSpliterator(s)
  }

  implicit class DoubleSpliteratorHasStepper(s: Spliterator.OfDouble) {
    def stepper: DoubleStepper = Stepper.ofSpliterator(s)
  }

  implicit class IntSpliteratorHasStepper(s: Spliterator.OfInt) {
    def stepper: IntStepper = Stepper.ofSpliterator(s)
  }

  implicit class LongSpliteratorHasStepper(s: Spliterator.OfLong) {
    def stepper: LongStepper = Stepper.ofSpliterator(s)
  }
}
