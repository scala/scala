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
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

@RunWith(classOf[JUnit4])
class StreamConvertersTest {

  @Test
  def convertStreamToScala(): Unit = {
    import collection.JavaConverters._
    import java.util.stream._

    for (par <- List(false, true)) {
      def is = { val s = Vector(1).seqStream; if (par) s.parallel else s }
      (is: IntStream).sum()
      def isbj: Stream[Integer] = is.boxed()
      def isbs: Stream[Int] = isbj.asInstanceOf[Stream[Int]]

      val isJavaUnboxed = isbj.unboxed
      (isJavaUnboxed: IntStream).sum
      val isUnboxed = isbs.unboxed
      (isUnboxed: IntStream).sum
      // Vector("").seqStream.unboxed // should not and does not compile, no unboxer available

      val isa = is.toScala(Accumulator)
      (isa: IntAccumulator).sum
      val isbja = isbj.toScala(Accumulator)
      (isbja: IntAccumulator).sum
      val isbsa = isbs.toScala(Accumulator)
      (isbsa: IntAccumulator).sum

      val isi = is.toScala(IntAccumulator)
      (isi: IntAccumulator).sum
      val isbji = isbj.toScala(IntAccumulator)
      (isbji: IntAccumulator).sum
      val isbsi = isbs.toScala(IntAccumulator)
      (isbsi: IntAccumulator).sum

      val isg = is.toScala(AnyAccumulator)
      (isg: AnyAccumulator[Int]).sum
      val isbjg = isbj.toScala(AnyAccumulator)
      (isbjg: AnyAccumulator[Integer]).asInstanceOf[AnyAccumulator[Int]].sum
      val isbsg = isbs.toScala(AnyAccumulator)
      (isbsg: AnyAccumulator[Int]).sum

      val isv = is.toScala(Vector)
      (isv: Vector[Int]).sum
      val isbjv = isbj.toScala(Vector)
      (isbjv: Vector[Integer]).asInstanceOf[Vector[Int]].sum
      val isbsv = isbs.toScala(Vector)
      (isbsv: Vector[Int]).sum

      var changer = 0
      val isbspll = isbs.parallel.map[Int](x => {changer += 1; x}).toScala(LazyList)
      assertEquals(1, changer) // parallel streams are converted to an (eager) Accumulator first
      assertEquals(1, (isbspll: LazyList[Int]).sum)
      assertEquals(1, changer)

      val isbsqll = isbs.sequential.map[Int](x => {changer += 1; x}).toScala(LazyList)
      assertEquals(1, changer) // sequential streams are converted lazily to lazy collections
      assertEquals(1, (isbsqll: LazyList[Int]).sum)
      assertEquals(2, changer)
    }
  }

  @Test
  def convertToAccumulator(): Unit = {
    val il = List(1)
    val bl = List(1.toByte)
    val sl = List("")

    val ia = Array(1)
    val sa = Array("")

    val ila1 = il.to(AnyAccumulator)
    (ila1: AnyAccumulator[Int]).clear()
    val ila2 = il.to(IntAccumulator)
    (ila2: IntAccumulator).clear()
    val ila3 = il.to(Accumulator)
    (ila3: IntAccumulator).clear()
    val bla1 = bl.to(AnyAccumulator)
    (bla1: AnyAccumulator[Byte]).clear()
//    val bla2 = bl.to(IntAccumulator) // Could implement a WideningAccumulator to support that.
    val bla2 = bl.to(Accumulator)
    (bla2: AnyAccumulator[Byte]).clear()

    val sla1 = sl.to(AnyAccumulator)
    (sla1: AnyAccumulator[String]).clear()
    val sla2 = sl.to(Accumulator)
    (sla2: AnyAccumulator[String]).clear()

    val iaa1 = ia.to(AnyAccumulator)
    (iaa1: AnyAccumulator[Int]).clear()
    val iaa2 = ia.to(IntAccumulator)
    (iaa2: IntAccumulator).clear()
    val iaa3 = ia.to(Accumulator)
    (iaa3: IntAccumulator).clear()

    val saa1 = sa.to(AnyAccumulator)
    (saa1: AnyAccumulator[String]).clear()
    val saa2 = sa.to(Accumulator)
    (saa2: AnyAccumulator[String]).clear()

    val is = il.stepper
    val ss = sl.stepper

    val isa1 = is.to(AnyAccumulator)
    (isa1: AnyAccumulator[Int]).clear()
    val isa2 = is.to(IntAccumulator)
    (isa2: IntAccumulator).clear()
    val isa3 = is.to(Accumulator)
    (isa3: IntAccumulator).clear()

    val ssa1 = ss.to(AnyAccumulator)
    (ssa1: AnyAccumulator[String]).clear()
    val ssa2 = ss.to(Accumulator)
    (ssa2: AnyAccumulator[String]).clear()
  }

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
