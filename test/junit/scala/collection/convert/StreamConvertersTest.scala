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

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StreamConvertersTest {

  // check that `stepper` returns the right type
  // check that `seqStream` and `parStream` extension methods are available and have the right type
  @Test
  def stepperSeqStreamParStreamMethods(): Unit = {
    import java.util.stream._

    val il = List(1,2,3)
    val iv = Vector(1,2,3)
    val ia = Array(1,2,3)
    val bv = Vector(1.toByte)
    val ba = Array(1.toByte)
    val sl = List("")
    val sv = Vector("")
    val sa = Array("")


    locally {
      val ils = il.stepper
      (ils: IntStepper /*with EfficientSubstep*/).nextStep(): Int
      val ivs = iv.stepper
      (ivs: IntStepper with EfficientSubstep).nextStep(): Int
      val ias = ia.stepper
      (ias: IntStepper with EfficientSubstep).nextStep(): Int
      val bvs = bv.stepper
      (bvs: IntStepper with EfficientSubstep).nextStep(): Int
      val bas = ba.stepper
      (bas: IntStepper with EfficientSubstep).nextStep(): Int
      val sls = sl.stepper
      (sls: AnyStepper[String] /*with EfficientSubstep*/).nextStep(): String
      val svs = sv.stepper
      (svs: AnyStepper[String] with EfficientSubstep).nextStep(): String
      val sas = sa.stepper
      (sas: AnyStepper[String] with EfficientSubstep).nextStep(): String
    }

    locally {
      import collection.JavaConverters._
      val ils = il.stepper
      (ils: IntStepper /*with EfficientSubstep*/).nextStep(): Int
      val ivs = iv.stepper
      (ivs: IntStepper with EfficientSubstep).nextStep(): Int
      val ias = ia.stepper
      (ias: IntStepper with EfficientSubstep).nextStep(): Int
      val bvs = bv.stepper
      (bvs: IntStepper with EfficientSubstep).nextStep(): Int
      val bas = ba.stepper
      (bas: IntStepper with EfficientSubstep).nextStep(): Int
      val sls = sl.stepper
      (sls: AnyStepper[String] /*with EfficientSubstep*/).nextStep(): String
      val svs = sv.stepper
      (svs: AnyStepper[String] with EfficientSubstep).nextStep(): String
      val sas = sa.stepper
      (sas: AnyStepper[String] with EfficientSubstep).nextStep(): String

      val ilq = il.seqStream
      (ilq: IntStream).sum()
      // val ilp = il.parStream // Not available
      val ivq = iv.seqStream
      (ivq: IntStream).sum()
      val ivr = iv.parStream
      (ivr: IntStream).sum()
      val iaq = ia.seqStream
      (iaq: IntStream).sum()
      val iar = ia.parStream
      (iar: IntStream).sum()

      val bvq = bv.seqStream
      (bvq: IntStream).sum()
      val bvr = bv.parStream
      (bvr: IntStream).sum()
      // val baq = ba.seqStream // Not available - though .stepper.seqStream is (tested below)
      // val bar = ba.parStream // Same

      val slq = sl.seqStream
      (slq: Stream[String]).count()
      // val slr = sl.parStream // Not available
      val svq = sv.seqStream
      (svq: Stream[String]).count()
      val svr = sv.parStream
      (svr: Stream[String]).count()
      val saq = sa.seqStream
      (saq: Stream[String]).count()
      val sar = sa.parStream
      (sar: Stream[String]).count()

      val ilsq = ils.seqStream
      (ilsq: IntStream).sum()
      // val ilsr = ils.parStream // Not available
      val ivsq = ivs.seqStream
      (ivsq: IntStream).sum()
      val ivsr = ivs.parStream
      (ivsr: IntStream).sum()
      val iasq = ias.seqStream
      (iasq: IntStream).sum()
      val iasr = ias.parStream
      (iasr: IntStream).sum()
      val bvsq = bvs.seqStream
      (bvsq: IntStream).sum()
      val bvsr = bvs.parStream
      (bvsr: IntStream).sum()
      val basq = bas.seqStream
      (basq: IntStream).sum()
      val basr = bas.parStream
      (basr: IntStream).sum()
      val slsq = sls.seqStream
      (slsq: Stream[String]).count()
      // val slsr = sls.parStream // Not available
      val svsq = svs.seqStream
      (svsq: Stream[String]).count()
      val svsr = svs.parStream
      (svsr: Stream[String]).count()
      val sasq = sas.seqStream
      (sasq: Stream[String]).count()
      val sasr = sas.parStream
      (sasr: Stream[String]).count()
    }
  }
}
