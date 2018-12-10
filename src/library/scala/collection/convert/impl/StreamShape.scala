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
package impl

import java.util.stream._

sealed trait StreamShape[T, S <: BaseStream[_, S], St <: Stepper[_]] {
  final def fromStepper(st: St, par: Boolean): S = stream(st, par)

  private def stream(st: St, par: Boolean): S = mkStream(if (par) st.anticipateParallelism() else st, par)
  protected def mkStream(st: St, par: Boolean): S
}

object StreamShape extends StreamShapeLowPriority1 {
  // primitive
  implicit val intStreamShape   : StreamShape[Int   , IntStream   , IntStepper]    = mkIntStreamShape[Int]
  implicit val longStreamShape  : StreamShape[Long  , LongStream  , LongStepper]   = mkLongStreamShape[Long]
  implicit val doubleStreamShape: StreamShape[Double, DoubleStream, DoubleStepper] = mkDoubleStreamShape[Double]
}

trait StreamShapeLowPriority1 extends StreamShapeLowPriority2 {

  // widening
  implicit val byteStreamShape : StreamShape[Byte , IntStream   , IntStepper]    = mkIntStreamShape[Byte]
  implicit val shortStreamShape: StreamShape[Short, IntStream   , IntStepper]    = mkIntStreamShape[Short]
  implicit val charStreamShape : StreamShape[Char , IntStream   , IntStepper]    = mkIntStreamShape[Char]
  implicit val floatStreamShape: StreamShape[Float, DoubleStream, DoubleStepper] = mkDoubleStreamShape[Float]

  protected def mkIntStreamShape[T]: StreamShape[T, IntStream, IntStepper] = new StreamShape[T, IntStream, IntStepper] {
    protected def mkStream(st: IntStepper, par: Boolean): IntStream = StreamSupport.intStream(st, par)
  }

  protected def mkLongStreamShape[T]: StreamShape[T, LongStream, LongStepper] = new StreamShape[T, LongStream, LongStepper] {
    protected def mkStream(st: LongStepper, par: Boolean): LongStream = StreamSupport.longStream(st, par)
  }

  protected def mkDoubleStreamShape[T]: StreamShape[T, DoubleStream, DoubleStepper] = new StreamShape[T, DoubleStream, DoubleStepper] {
    protected def mkStream(st: DoubleStepper, par: Boolean): DoubleStream = StreamSupport.doubleStream(st, par)
  }
}

trait StreamShapeLowPriority2 {
  // reference
  implicit def anyStreamShape[T]: StreamShape[T, Stream[T], AnyStepper[T]] = anyStreamShape.asInstanceOf[StreamShape[T, Stream[T], AnyStepper[T]]]

  private[this] val anyStreamShape: StreamShape[AnyRef, Stream[AnyRef], AnyStepper[AnyRef]] = new StreamShape[AnyRef, Stream[AnyRef], AnyStepper[AnyRef]] {
    def mkStream(s: AnyStepper[AnyRef], par: Boolean): Stream[AnyRef] = StreamSupport.stream(s, par)
  }
}
