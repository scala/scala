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

package scala.collection.convert.impl

import scala.collection.convert._

sealed trait AccumulatorFactoryShape[A, C] {
  def factory: collection.Factory[A, C]
  def empty: C
}

object AccumulatorFactoryShape extends LowPriorityAccumulatorFactoryShape {
  implicit val doubleAccumulatorFactoryShape: AccumulatorFactoryShape[Double, DoubleAccumulator] = new AccumulatorFactoryShape[Double, DoubleAccumulator] {
    def factory: collection.Factory[Double, DoubleAccumulator] = DoubleAccumulator
    def empty: DoubleAccumulator = DoubleAccumulator.empty
  }

  implicit val intAccumulatorFactoryShape: AccumulatorFactoryShape[Int, IntAccumulator] = new AccumulatorFactoryShape[Int, IntAccumulator] {
    def factory: collection.Factory[Int, IntAccumulator] = IntAccumulator
    def empty: IntAccumulator = IntAccumulator.empty
  }

  implicit val longAccumulatorFactoryShape: AccumulatorFactoryShape[Long, LongAccumulator] = new AccumulatorFactoryShape[Long, LongAccumulator] {
    def factory: collection.Factory[Long, LongAccumulator] = LongAccumulator
    def empty: LongAccumulator = LongAccumulator.empty
  }

  implicit val javaDoubleAccumulatorFactoryShape: AccumulatorFactoryShape[java.lang.Double, DoubleAccumulator] = doubleAccumulatorFactoryShape.asInstanceOf[AccumulatorFactoryShape[java.lang.Double, DoubleAccumulator]]
  implicit val javaIntegerAccumulatorFactoryShape: AccumulatorFactoryShape[java.lang.Integer, IntAccumulator] = intAccumulatorFactoryShape.asInstanceOf[AccumulatorFactoryShape[java.lang.Integer, IntAccumulator]]
  implicit val javaLongAccumulatorFactoryShape: AccumulatorFactoryShape[java.lang.Long, LongAccumulator] = longAccumulatorFactoryShape.asInstanceOf[AccumulatorFactoryShape[java.lang.Long, LongAccumulator]]
}

sealed trait LowPriorityAccumulatorFactoryShape {
  implicit def anyAccumulatorFactoryShape[A]: AccumulatorFactoryShape[A, AnyAccumulator[A]] = anyAccumulatorFactoryShapePrototype.asInstanceOf[AccumulatorFactoryShape[A, AnyAccumulator[A]]]

  private val anyAccumulatorFactoryShapePrototype = new AccumulatorFactoryShape[AnyRef, AnyAccumulator[AnyRef]] {
    def factory: collection.Factory[AnyRef, AnyAccumulator[AnyRef]] = collection.IterableFactory.toFactory(AnyAccumulator)
    def empty: AnyAccumulator[AnyRef] = AnyAccumulator.empty[AnyRef]
  }
}
