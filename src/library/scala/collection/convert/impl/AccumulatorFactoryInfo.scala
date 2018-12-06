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

  implicit val javaIntegerAccumulatorFactoryInfo: AccumulatorFactoryInfo[java.lang.Integer, IntAccumulator] = intAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[java.lang.Integer, IntAccumulator]]
  implicit val javaLongAccumulatorFactoryInfo: AccumulatorFactoryInfo[java.lang.Long, IntAccumulator] = longAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[java.lang.Long, IntAccumulator]]
  implicit val javaDoubleAccumulatorFactoryInfo: AccumulatorFactoryInfo[java.lang.Double, IntAccumulator] = doubleAccumulatorFactoryInfo.asInstanceOf[AccumulatorFactoryInfo[java.lang.Double, IntAccumulator]]
}
