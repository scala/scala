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

import java.util.stream.BaseStream

import scala.collection.AnyConstr
import scala.collection.convert.impl.{StepperShape, StreamShape}

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
}
