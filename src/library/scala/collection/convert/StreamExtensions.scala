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

package scala.collection.convert

import java.util.Spliterator
import java.util.stream._
import java.{lang => jl}

import scala.annotation.implicitNotFound
import scala.collection.Stepper.EfficientSplit
import scala.collection._
import scala.collection.convert.StreamExtensions.{AccumulatorFactoryInfo, StreamShape, StreamUnboxer}
import scala.jdk.CollectionConverters._
import scala.jdk._

/** Defines extension methods to create Java Streams for Scala collections, available through
  * [[scala.jdk.javaapi.StreamConverters]].
  */
trait StreamExtensions {
  // collections

  implicit class IterableHasSeqStream[A](cc: IterableOnce[A]) {
    /** Create a sequential [[java.util.stream.Stream Java Stream]] for this collection. If the
      * collection contains primitive values, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaSeqStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S =
      s.fromStepper(cc.stepper, par = false)
  }

  // Not `CC[X] <: IterableOnce[X]`, but `C` with an extra constraint, to support non-parametric classes like IntAccumulator
  implicit class IterableNonGenericHasParStream[A, C <: IterableOnce[_]](c: C)(implicit ev: C <:< IterableOnce[A]) {
    private type IterableOnceWithEfficientStepper = IterableOnce[A] {
      def stepper[S <: Stepper[_]](implicit shape : StepperShape[A, S]) : S with EfficientSplit
    }

    /** Create a parallel [[java.util.stream.Stream Java Stream]] for this collection. If the
      * collection contains primitive values, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaParStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit
        s: StreamShape[A, S, St],
        st: StepperShape[A, St],
        @implicitNotFound("`parStream` can only be called on collections where `stepper` returns a `Stepper with EfficientSplit`")
        isEfficient: C <:< IterableOnceWithEfficientStepper): S =
      s.fromStepper(ev(c).stepper, par = true)
  }

  // maps

  implicit class MapHasSeqKeyValueStream[K, V, CC[X, Y] <: collection.MapOps[X, Y, collection.Map, _]](cc: CC[K, V]) {
    /** Create a sequential [[java.util.stream.Stream Java Stream]] for the keys of this map. If
      * the keys are primitive values, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaSeqKeyStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit s: StreamShape[K, S, St], st: StepperShape[K, St]): S =
      s.fromStepper(cc.keyStepper, par = false)

    /** Create a sequential [[java.util.stream.Stream Java Stream]] for the values of this map. If
      * the values are primitives, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaSeqValueStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit s: StreamShape[V, S, St], st: StepperShape[V, St]): S =
      s.fromStepper(cc.valueStepper, par = false)

    // The asJavaSeqStream extension method for IterableOnce doesn't apply because its `CC` takes a single type parameter, whereas the one here takes two
    /** Create a sequential [[java.util.stream.Stream Java Stream]] for the `(key, value)` pairs of
      * this map.
      */
    def asJavaSeqStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit s: StreamShape[(K, V), S, St], st: StepperShape[(K, V), St]): S =
      s.fromStepper(cc.stepper, par = false)
  }


  implicit class MapHasParKeyValueStream[K, V, CC[X, Y] <: collection.MapOps[X, Y, collection.Map, _]](cc: CC[K, V]) {
    private type MapOpsWithEfficientKeyStepper = collection.MapOps[K, V, collection.Map, _] { def keyStepper[S <: Stepper[_]](implicit shape : StepperShape[K, S]) : S with EfficientSplit }
    private type MapOpsWithEfficientValueStepper = collection.MapOps[K, V, collection.Map, _] { def valueStepper[S <: Stepper[_]](implicit shape : StepperShape[V, S]) : S with EfficientSplit }
    private type MapOpsWithEfficientStepper = collection.MapOps[K, V, collection.Map, _] { def stepper[S <: Stepper[_]](implicit shape : StepperShape[(K, V), S]) : S with EfficientSplit }

    /** Create a parallel [[java.util.stream.Stream Java Stream]] for the keys of this map. If
      * the keys are primitive values, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaParKeyStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit
        s: StreamShape[K, S, St],
        st: StepperShape[K, St],
        @implicitNotFound("parKeyStream can only be called on maps where `keyStepper` returns a `Stepper with EfficientSplit`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientKeyStepper): S =
      s.fromStepper(cc.keyStepper, par = true)

    /** Create a parallel [[java.util.stream.Stream Java Stream]] for the values of this map. If
      * the values are primitives, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaParValueStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit
        s: StreamShape[V, S, St],
        st: StepperShape[V, St],
        @implicitNotFound("parValueStream can only be called on maps where `valueStepper` returns a `Stepper with EfficientSplit`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientValueStepper): S =
      s.fromStepper(cc.valueStepper, par = true)

    // The asJavaParStream extension method for IterableOnce doesn't apply because its `CC` takes a single type parameter, whereas the one here takes two
    /** Create a parallel [[java.util.stream.Stream Java Stream]] for the `(key, value)` pairs of
      * this map.
      */
    def asJavaParStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit
        s: StreamShape[(K, V), S, St],
        st: StepperShape[(K, V), St],
        @implicitNotFound("parStream can only be called on maps where `stepper` returns a `Stepper with EfficientSplit`")
        isEfficient: CC[K, V] <:< MapOpsWithEfficientStepper): S =
      s.fromStepper(cc.stepper, par = true)
  }

  // steppers

  implicit class StepperHasSeqStream[A](stepper: Stepper[A]) {
    /** Create a sequential [[java.util.stream.Stream Java Stream]] for this stepper. If the
      * stepper yields primitive values, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaSeqStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S = {
      val sStepper = stepper match {
        case as: AnyStepper[A] => st.seqUnbox(as)
        case _ => stepper.asInstanceOf[St]
      }
      s.fromStepper(sStepper, par = false)
    }
  }

  implicit class StepperHasParStream[A](stepper: Stepper[A] with EfficientSplit) {
    /** Create a parallel [[java.util.stream.Stream Java Stream]] for this stepper. If the
      * stepper yields primitive values, a corresponding specialized Stream is returned (e.g.,
      * [[java.util.stream.IntStream `IntStream`]]).
      */
    def asJavaParStream[S <: BaseStream[_, _], St <: Stepper[_]](implicit s: StreamShape[A, S, St], st: StepperShape[A, St]): S = {
      val sStepper = stepper match {
        case as: AnyStepper[A] with EfficientSplit => st.parUnbox(as)
        case _ => stepper.asInstanceOf[St]
      }
      s.fromStepper(sStepper, par = true)
    }
  }

  // arrays
  // uses the JDK array spliterators (`DoubleArraySpliterator`). users can also call
  // `array.stepper.seqStream`, which then uses the Scala steppers (`DoubleArrayStepper`). the
  // steppers are also available on byte/short/char/float arrays (`WidenedByteArrayStepper`),
  // JDK spliterators only for double/int/long/reference.

  implicit class DoubleArrayHasSeqParStream(a: Array[Double]) {
    /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaSeqStream: DoubleStream = java.util.Arrays.stream(a)
    /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaParStream: DoubleStream = asJavaSeqStream.parallel
  }

  implicit class IntArrayHasSeqParStream(a: Array[Int]) {
    /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = java.util.Arrays.stream(a)
    /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = asJavaSeqStream.parallel
  }

  implicit class LongArrayHasSeqParStream(a: Array[Long]) {
    /** Create a sequential [[java.util.stream.LongStream Java LongStream]] for this array. */
    def asJavaSeqStream: LongStream = java.util.Arrays.stream(a)
    /** Create a parallel [[java.util.stream.LongStream Java LongStream]] for this array. */
    def asJavaParStream: LongStream = asJavaSeqStream.parallel
  }

  implicit class AnyArrayHasSeqParStream[A <: AnyRef](a: Array[A]) {
    /** Create a sequential [[java.util.stream.Stream Java Stream]] for this array. */
    def asJavaSeqStream: Stream[A] = java.util.Arrays.stream(a)
    /** Create a parallel [[java.util.stream.Stream Java Stream]] for this array. */
    def asJavaParStream: Stream[A] = asJavaSeqStream.parallel
  }

  implicit class ByteArrayHasSeqParStream(a: Array[Byte]) {
    /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = a.stepper.asJavaSeqStream
    /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = a.stepper.asJavaParStream
  }

  implicit class ShortArrayHasSeqParStream(a: Array[Short]) {
    /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = a.stepper.asJavaSeqStream
    /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = a.stepper.asJavaParStream
  }

  implicit class CharArrayHasSeqParStream(a: Array[Char]) {
    /** Create a sequential [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaSeqStream: IntStream = a.stepper.asJavaSeqStream
    /** Create a parallel [[java.util.stream.IntStream Java IntStream]] for this array. */
    def asJavaParStream: IntStream = a.stepper.asJavaParStream
  }

  implicit class FloatArrayHasSeqParStream(a: Array[Float]) {
    /** Create a sequential [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaSeqStream: DoubleStream = a.stepper.asJavaSeqStream
    /** Create a parallel [[java.util.stream.DoubleStream Java DoubleStream]] for this array. */
    def asJavaParStream: DoubleStream = a.stepper.asJavaParStream
  }



  // strings

  implicit class StringHasSeqParStream(s: String) {
    /**
     * A sequential stream on the characters of a string, same as [[asJavaSeqCharStream]]. See also
     * [[asJavaSeqCodePointStream]].
     */
    def asJavaSeqStream: IntStream = StreamSupport.intStream(s.stepper.spliterator, /* par = */ false)
    /**
     * A parallel stream on the characters of a string, same as [[asJavaParCharStream]]. See also
     * [[asJavaParCodePointStream]].
     */
    def asJavaParStream: IntStream = StreamSupport.intStream(s.stepper.spliterator, /* par = */ true)

    /** A sequential stream on the characters of a string. See also  [[asJavaSeqCodePointStream]]. */
    def asJavaSeqCharStream: IntStream = StreamSupport.intStream(s.charStepper.spliterator, /* par = */ false)
    /** A parallel stream on the characters of a string. See also [[asJavaParCodePointStream]]. */
    def asJavaParCharStream: IntStream = StreamSupport.intStream(s.charStepper.spliterator, /* par = */ true)

    /** A sequential stream on the code points of a string. See also [[asJavaSeqCharStream]]. */
    def asJavaSeqCodePointStream: IntStream = StreamSupport.intStream(s.codePointStepper.spliterator, /* par = */ false)
    /** A parallel stream on the code points of a string. See also [[asJavaParCharStream]]. */
    def asJavaParCodePointStream: IntStream = StreamSupport.intStream(s.codePointStepper.spliterator, /* par = */ true)
  }

  // toScala for streams

  implicit class StreamHasToScala[A](stream: Stream[A]) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts streams of boxed integers, longs or
     * doubles are converted to the primitive accumulators ([[scala.jdk.IntAccumulator]], etc.).
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
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
      else factory.fromSpecific(stream.iterator.asScala)
    }

    /** Convert a generic Java Stream wrapping a primitive type to a corresponding primitive
      * Stream.
      */
    def asJavaPrimitiveStream[S](implicit unboxer: StreamUnboxer[A, S]): S = unboxer(stream)
  }

  implicit class IntStreamHasToScala(stream: IntStream) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts the `IntStream` to a primitive
     * [[scala.jdk.IntAccumulator]].
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
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
      else factory.fromSpecific(stream.iterator.asInstanceOf[java.util.Iterator[Int]].asScala)
    }
  }

  implicit class LongStreamHasToScala(stream: LongStream) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts the `LongStream` to a primitive
     * [[scala.jdk.LongAccumulator]].
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
     * then converted to the target collection. Note that the stream is processed eagerly while
     * building the accumulator, even if the target collection is lazy.
     *
     * Sequential streams are directly converted to the target collection. If the target collection
     * is lazy, the conversion is lazy as well.
     */
    def toScala[C1](factory: collection.Factory[Long, C1])(implicit info: AccumulatorFactoryInfo[Long, C1]): C1 = {
      def longAcc = stream.collect(LongAccumulator.supplier, LongAccumulator.adder, LongAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Long], AnyAccumulator.unboxedLongAdder, AnyAccumulator.merger[Long]).asInstanceOf[C1]
      else if (info.companion == LongAccumulator) longAcc.asInstanceOf[C1]
      else if (stream.isParallel) longAcc.to(factory)
      else factory.fromSpecific(stream.iterator.asInstanceOf[java.util.Iterator[Long]].asScala)
    }
  }

  implicit class DoubleStreamHasToScala(stream: DoubleStream) {
    /**
     * Copy the elements of this stream into a Scala collection.
     *
     * Converting a parallel streams to an [[scala.jdk.Accumulator]] using `stream.toScala(Accumulator)`
     * builds the result in parallel.
     *
     * A `toScala(Accumulator)` call automatically converts the `DoubleStream` to a primitive
     * [[scala.jdk.DoubleAccumulator]].
     *
     * When converting a parallel stream to a different Scala collection, the stream is first
     * converted into an [[scala.jdk.Accumulator]], which supports parallel building. The accumulator is
     * then converted to the target collection. Note that the stream is processed eagerly while
     * building the accumulator, even if the target collection is lazy.
     *
     * Sequential streams are directly converted to the target collection. If the target collection
     * is lazy, the conversion is lazy as well.
     */
    def toScala[C1](factory: collection.Factory[Double, C1])(implicit info: AccumulatorFactoryInfo[Double, C1]): C1 = {
      def doubleAcc = stream.collect(DoubleAccumulator.supplier, DoubleAccumulator.adder, DoubleAccumulator.merger)
      if (info.companion == AnyAccumulator) stream.collect(AnyAccumulator.supplier[Double], AnyAccumulator.unboxedDoubleAdder, AnyAccumulator.merger[Double]).asInstanceOf[C1]
      else if (info.companion == DoubleAccumulator) doubleAcc.asInstanceOf[C1]
      else if (stream.isParallel) doubleAcc.to(factory)
      else factory.fromSpecific(stream.iterator.asInstanceOf[java.util.Iterator[Double]].asScala)
    }
  }
}

object StreamExtensions {
  /** An implicit StreamShape instance connects element types with the corresponding specialized
    * Stream and Stepper types. This is used in `asJavaStream` extension methods to create
    * generic or primitive streams according to the element type.
    */
  sealed trait StreamShape[T, S <: BaseStream[_, _], St <: Stepper[_]] {
    final def fromStepper(st: St, par: Boolean): S = mkStream(st, par)
    protected def mkStream(st: St, par: Boolean): S
  }

  object StreamShape extends StreamShapeLowPriority1 {
    // primitive
    implicit val intStreamShape   : StreamShape[Int   , IntStream   , IntStepper]    = mkIntStreamShape[Int]
    implicit val longStreamShape  : StreamShape[Long  , LongStream  , LongStepper]   = mkLongStreamShape[Long]
    implicit val doubleStreamShape: StreamShape[Double, DoubleStream, DoubleStepper] = mkDoubleStreamShape[Double]

    // widening
    implicit val byteStreamShape : StreamShape[Byte , IntStream   , IntStepper]    = mkIntStreamShape[Byte]
    implicit val shortStreamShape: StreamShape[Short, IntStream   , IntStepper]    = mkIntStreamShape[Short]
    implicit val charStreamShape : StreamShape[Char , IntStream   , IntStepper]    = mkIntStreamShape[Char]
    implicit val floatStreamShape: StreamShape[Float, DoubleStream, DoubleStepper] = mkDoubleStreamShape[Float]

    // boxed java primitives

    implicit val jIntegerStreamShape   : StreamShape[jl.Integer  , IntStream   , IntStepper   ] = mkIntStreamShape[jl.Integer]
    implicit val jLongStreamShape      : StreamShape[jl.Long     , LongStream  , LongStepper  ] = mkLongStreamShape[jl.Long]
    implicit val jDoubleStreamShape    : StreamShape[jl.Double   , DoubleStream, DoubleStepper] = mkDoubleStreamShape[jl.Double]
    implicit val jByteStreamShape      : StreamShape[jl.Byte     , IntStream   , IntStepper   ] = mkIntStreamShape[jl.Byte]
    implicit val jShortStreamShape     : StreamShape[jl.Short    , IntStream   , IntStepper   ] = mkIntStreamShape[jl.Short]
    implicit val jCharacterStreamShape : StreamShape[jl.Character, IntStream   , IntStepper   ] = mkIntStreamShape[jl.Character]
    implicit val jFloatStreamShape     : StreamShape[jl.Float    , DoubleStream, DoubleStepper] = mkDoubleStreamShape[jl.Float]

    private def mkIntStreamShape[T]: StreamShape[T, IntStream, IntStepper] = new StreamShape[T, IntStream, IntStepper] {
      protected def mkStream(st: IntStepper, par: Boolean): IntStream = StreamSupport.intStream(st.spliterator, par)
    }

    private def mkLongStreamShape[T]: StreamShape[T, LongStream, LongStepper] = new StreamShape[T, LongStream, LongStepper] {
      protected def mkStream(st: LongStepper, par: Boolean): LongStream = StreamSupport.longStream(st.spliterator, par)
    }

    private def mkDoubleStreamShape[T]: StreamShape[T, DoubleStream, DoubleStepper] = new StreamShape[T, DoubleStream, DoubleStepper] {
      protected def mkStream(st: DoubleStepper, par: Boolean): DoubleStream = StreamSupport.doubleStream(st.spliterator, par)
    }
  }

  trait StreamShapeLowPriority1 {
    // reference
    implicit def anyStreamShape[T]: StreamShape[T, Stream[T], Stepper[T]] = anyStreamShapePrototype.asInstanceOf[StreamShape[T, Stream[T], Stepper[T]]]

    private[this] val anyStreamShapePrototype: StreamShape[AnyRef, Stream[AnyRef], Stepper[AnyRef]] = new StreamShape[AnyRef, Stream[AnyRef], Stepper[AnyRef]] {
      def mkStream(s: Stepper[AnyRef], par: Boolean): Stream[AnyRef] = StreamSupport.stream(s.spliterator.asInstanceOf[Spliterator[AnyRef]], par)
    }
  }

  /** Connects a stream element type `A` to the corresponding, potentially specialized, Stream type.
    * Used in the `stream.asJavaPrimitiveStream` extension method.
    */
  sealed trait StreamUnboxer[A, S] {
    def apply(s: Stream[A]): S
  }
  object StreamUnboxer {
    implicit val intStreamUnboxer: StreamUnboxer[Int, IntStream] = new StreamUnboxer[Int, IntStream] {
      def apply(s: Stream[Int]): IntStream = s.mapToInt(x => x)
    }
    implicit val javaIntegerStreamUnboxer: StreamUnboxer[jl.Integer, IntStream] = intStreamUnboxer.asInstanceOf[StreamUnboxer[jl.Integer, IntStream]]

    implicit val longStreamUnboxer: StreamUnboxer[Long, LongStream] = new StreamUnboxer[Long, LongStream] {
      def apply(s: Stream[Long]): LongStream = s.mapToLong(x => x)
    }
    implicit val javaLongStreamUnboxer: StreamUnboxer[jl.Long, LongStream] = longStreamUnboxer.asInstanceOf[StreamUnboxer[jl.Long, LongStream]]

    implicit val doubleStreamUnboxer: StreamUnboxer[Double, DoubleStream] = new StreamUnboxer[Double, DoubleStream] {
      def apply(s: Stream[Double]): DoubleStream = s.mapToDouble(x => x)
    }
    implicit val javaDoubleStreamUnboxer: StreamUnboxer[jl.Double, DoubleStream] = doubleStreamUnboxer.asInstanceOf[StreamUnboxer[jl.Double, DoubleStream]]
  }



  /** An implicit `AccumulatorFactoryInfo` connects primitive element types to the corresponding
    * specialized [[scala.jdk.Accumulator]] factory. This is used in the `stream.toScala` extension methods
    * to ensure collecting a primitive stream into a primitive accumulator does not box.
    *
    * When converting to a collection other than `Accumulator`, the generic
    * `noAccumulatorFactoryInfo` is passed.
    */
  trait AccumulatorFactoryInfo[A, C] {
    val companion: AnyRef
  }
  trait LowPriorityAccumulatorFactoryInfo {
    implicit def noAccumulatorFactoryInfo[A, C]: AccumulatorFactoryInfo[A, C] = noAccumulatorFactoryInfoPrototype.asInstanceOf[AccumulatorFactoryInfo[A, C]]
    private val noAccumulatorFactoryInfoPrototype: AccumulatorFactoryInfo[AnyRef, AnyRef] = new AccumulatorFactoryInfo[AnyRef, AnyRef] {
      val companion: AnyRef = null
    }
  }
  object AccumulatorFactoryInfo extends LowPriorityAccumulatorFactoryInfo {
    implicit def anyAccumulatorFactoryInfo[A]: AccumulatorFactoryInfo[A, AnyAccumulator[A]] = anyAccumulatorFactoryInfoPrototype.asInstanceOf[AccumulatorFactoryInfo[A, AnyAccumulator[A]]]

    private object anyAccumulatorFactoryInfoPrototype extends AccumulatorFactoryInfo[AnyRef, AnyAccumulator[AnyRef]] {
      val companion: AnyRef = AnyAccumulator
    }

    implicit val intAccumulatorFactoryInfo: AccumulatorFactoryInfo[Int, IntAccumulator] = new AccumulatorFactoryInfo[Int, IntAccumulator] {
      val companion: AnyRef = IntAccumulator
    }

    implicit val longAccumulatorFactoryInfo: AccumulatorFactoryInfo[Long, LongAccumulator] = new AccumulatorFactoryInfo[Long, LongAccumulator] {
      val companion: AnyRef = LongAccumulator
    }

    implicit val doubleAccumulatorFactoryInfo: AccumulatorFactoryInfo[Double, DoubleAccumulator] = new AccumulatorFactoryInfo[Double, DoubleAccumulator] {
      val companion: AnyRef = DoubleAccumulator
    }

    implicit val jIntegerAccumulatorFactoryInfo: AccumulatorFactoryInfo[jl.Integer, IntAccumulator] = intAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[jl.Integer, IntAccumulator]]
    implicit val jLongAccumulatorFactoryInfo: AccumulatorFactoryInfo[jl.Long, IntAccumulator] = longAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[jl.Long, IntAccumulator]]
    implicit val jDoubleAccumulatorFactoryInfo: AccumulatorFactoryInfo[jl.Double, IntAccumulator] = doubleAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[jl.Double, IntAccumulator]]
  }
}
