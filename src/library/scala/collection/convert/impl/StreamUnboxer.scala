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

import java.util.stream._

sealed trait StreamUnboxer[A, S] {
  def apply(s: Stream[A]): S
}
object StreamUnboxer {
  implicit val intStreamUnboxer: StreamUnboxer[Int, IntStream] = new StreamUnboxer[Int, IntStream] {
    def apply(s: Stream[Int]): IntStream = s.mapToInt(x => x)
  }
  implicit val javaIntegerStreamUnboxer: StreamUnboxer[java.lang.Integer, IntStream] = intStreamUnboxer.asInstanceOf[StreamUnboxer[java.lang.Integer, IntStream]]

  implicit val longStreamUnboxer: StreamUnboxer[Long, LongStream] = new StreamUnboxer[Long, LongStream] {
    def apply(s: Stream[Long]): LongStream = s.mapToLong(x => x)
  }
  implicit val javaLongStreamUnboxer: StreamUnboxer[java.lang.Long, LongStream] = longStreamUnboxer.asInstanceOf[StreamUnboxer[java.lang.Long, LongStream]]

  implicit val doubleStreamUnboxer: StreamUnboxer[Double, DoubleStream] = new StreamUnboxer[Double, DoubleStream] {
    def apply(s: Stream[Double]): DoubleStream = s.mapToDouble(x => x)
  }
  implicit val javaDoubleStreamUnboxer: StreamUnboxer[java.lang.Double, DoubleStream] = doubleStreamUnboxer.asInstanceOf[StreamUnboxer[java.lang.Double, DoubleStream]]
}
