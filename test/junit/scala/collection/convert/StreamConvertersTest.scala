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

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection._

@RunWith(classOf[JUnit4])
class StreamConvertersTest {
  @Test
  def keyValueSteppers(): Unit = {
    import scala.jdk.StreamConverters.Ops._

    val m1 = Map(1 -> "a")
    val m2 = collection.mutable.LinkedHashMap('c' -> 35f)
    val s1 = Set("3", "4")
    val s2 = collection.mutable.LinkedHashSet('a', 'b')


    val m1ks = m1.keyStepper
    (m1ks: IntStepper /*with EfficientSubstep*/).nextStep()
    val m1vs = m1.valueStepper
    (m1vs: AnyStepper[String] /*with EfficientSubstep*/).nextStep()
    val m2ks = m2.keyStepper
    (m2ks: IntStepper with EfficientSubstep).nextStep()
    val m2vs = m2.valueStepper
    (m2vs: DoubleStepper with EfficientSubstep).nextStep()

    val m1sps = m1.asJavaSeqStream
    (m1sps: Stream[(Int, String)]).count()
    val m1sks = m1.asJavaSeqKeyStream
    (m1sks: IntStream).sum()
    val m1svs = m1.asJavaSeqValueStream
    (m1svs: Stream[String]).count()

    // val m1pps = m1.parStream // Not available
    // val m1pks = m1.parKeyStream // Not available
    // val m1pvs = m1.parValueStream // Not available

    val m2sps = m2.asJavaSeqStream
    (m2sps: Stream[(Char, Float)]).count()
    val m2sks = m2.asJavaSeqKeyStream
    (m2sks: IntStream).sum()
    val m2svs = m2.asJavaSeqValueStream
    (m2svs: DoubleStream).count()

    val m2pps = m2.asJavaParStream
    (m2pps: Stream[(Char, Float)]).count()
    val m2pks = m2.asJavaParKeyStream
    (m2pks: IntStream).sum()
    val m2pvs = m2.asJavaParValueStream
    (m2pvs: DoubleStream).count()

    val s1sps = s1.asJavaSeqStream
    (s1sps: Stream[String]).count()
//    val s1pps = s1.parStream

    val s2sps = s2.asJavaSeqStream
    (s2sps: IntStream).count()
    val s2pps = s2.asJavaParStream
    (s2pps: IntStream).count()
  }

  /*
  @Test
  def spliteratorHasStepper(): Unit = {
    import scala.jdk.StreamConverters.Ops._

    val ia = Array(1,2,3)
    val sa = Array("")

    val ias = ia.asJavaSeqStream.spliterator.asScalaStepper
    (ias: IntStepper).nextStep()
    val iasb = ias.asJavaSeqStream.boxed.spliterator.asScalaStepper
    (iasb: Stepper[Integer]).nextStep()
    val sas = sa.asJavaSeqStream.spliterator.asScalaStepper
    (sas: Stepper[String]).nextStep()
  }
  */

  @Test
  def toArrayTests(): Unit = {
    val ia = Array(1,2,3)
    val ba = Array[Byte](1,2,3)

    def f(a: Any) = ()

    /*
    val ias = ia.stepper
    (ias: IntStepper).toArray : Array[Int]
    val bas = ba.stepper
    (bas: IntStepper).toArray : Array[Int]
    */

    val iaa = ia.to(Accumulator)
    (iaa: IntAccumulator).toArray : Array[Int]
    val baa = ba.to(Accumulator)
    (baa: AnyAccumulator[Byte]).toArray : Array[Byte]
    /*
    val baai = ba.stepper.to(Accumulator)
    (baai: IntAccumulator).toArray : Array[Int]
    */
  }

  @Test
  def convertStreamToScala(): Unit = {
    import scala.jdk.StreamConverters.Ops._

    for (par <- List(false, true)) {
      def is = { val s = Vector(1).asJavaSeqStream; if (par) s.parallel else s }
      (is: IntStream).sum()
      def isbj: Stream[Integer] = is.boxed()
      def isbs: Stream[Int] = isbj.asInstanceOf[Stream[Int]]

      val isJavaUnboxed = isbj.asJavaPrimitiveStream
      (isJavaUnboxed: IntStream).sum
      val isUnboxed = isbs.asJavaPrimitiveStream
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

    /*
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
    */
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
      import scala.jdk.StreamConverters.Ops._
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

      val ilq = il.asJavaSeqStream
      (ilq: IntStream).sum()
      // val ilp = il.parStream // Not available
      val ivq = iv.asJavaSeqStream
      (ivq: IntStream).sum()
      val ivr = iv.asJavaParStream
      (ivr: IntStream).sum()
      val iaq = ia.asJavaSeqStream
      (iaq: IntStream).sum()
      val iar = ia.asJavaParStream
      (iar: IntStream).sum()

      val bvq = bv.asJavaSeqStream
      (bvq: IntStream).sum()
      val bvr = bv.asJavaParStream
      (bvr: IntStream).sum()
      // val baq = ba.seqStream // Not available - though .stepper.seqStream is (tested below)
      // val bar = ba.parStream // Same

      val slq = sl.asJavaSeqStream
      (slq: Stream[String]).count()
      // val slr = sl.parStream // Not available
      val svq = sv.asJavaSeqStream
      (svq: Stream[String]).count()
      val svr = sv.asJavaParStream
      (svr: Stream[String]).count()
      val saq = sa.asJavaSeqStream
      (saq: Stream[String]).count()
      val sar = sa.asJavaParStream
      (sar: Stream[String]).count()

      val ilsq = ils.asJavaSeqStream
      (ilsq: IntStream).sum()
      // val ilsr = ils.parStream // Not available
      val ivsq = ivs.asJavaSeqStream
      (ivsq: IntStream).sum()
      val ivsr = ivs.asJavaParStream
      (ivsr: IntStream).sum()
      val iasq = ias.asJavaSeqStream
      (iasq: IntStream).sum()
      val iasr = ias.asJavaParStream
      (iasr: IntStream).sum()
      val bvsq = bvs.asJavaSeqStream
      (bvsq: IntStream).sum()
      val bvsr = bvs.asJavaParStream
      (bvsr: IntStream).sum()
      val basq = bas.asJavaSeqStream
      (basq: IntStream).sum()
      val basr = bas.asJavaParStream
      (basr: IntStream).sum()
      val slsq = sls.asJavaSeqStream
      (slsq: Stream[String]).count()
      // val slsr = sls.parStream // Not available
      val svsq = svs.asJavaSeqStream
      (svsq: Stream[String]).count()
      val svsr = svs.asJavaParStream
      (svsr: Stream[String]).count()
      val sasq = sas.asJavaSeqStream
      (sasq: Stream[String]).count()
      val sasr = sas.asJavaParStream
      (sasr: Stream[String]).count()
    }
  }

  @Test
  def bitSetStepper(): Unit = {
    locally {
      val ibs = collection.immutable.BitSet(1, 2, 3)
      val s1 = ibs.stepper
      val s2: IntStepper = ibs.stepper
      val s3: IntStepper with EfficientSubstep = ibs.stepper
      val s4: Stepper[Int] = ibs.stepper
      val s5: Stepper[Int] with EfficientSubstep = ibs.stepper
      val s6: AnyStepper[Int] = ibs.stepper[Int, AnyStepper[Int]]
      val s7: AnyStepper[Int] with EfficientSubstep = ibs.stepper[Int, AnyStepper[Int]]
      // val s8: Stepper[Any] = ibs.stepper  // no StepperShape instance
      // val s9: Stepper[Long] = ibs.stepper // no StepperShape instance
    }

    locally {
      val mbs = collection.mutable.BitSet(1, 2, 3)
      val s1 = mbs.stepper
      val s2: IntStepper = mbs.stepper
      val s3: IntStepper with EfficientSubstep = mbs.stepper
      val s4: Stepper[Int] = mbs.stepper
      val s5: Stepper[Int] with EfficientSubstep = mbs.stepper
      val s6: AnyStepper[Int] = mbs.stepper(StepperShape.anyStepperShape[Int])
      val s7: AnyStepper[Int] with EfficientSubstep = mbs.stepper(convert.StepperShape.anyStepperShape[Int])
      // val s8: Stepper[Any] = mbs.stepper  // no StepperShape instance
      // val s9: Stepper[Long] = mbs.stepper // no StepperShape instance
    }
  }

  @Test
  def rangeStepper(): Unit = {
    val r = 1 to 20 by 3
    val s1 = r.stepper
    val s2: IntStepper = r.stepper
    val s3: IntStepper with EfficientSubstep = r.stepper
    val s4: Stepper[Int] = r.stepper
    val s5: Stepper[Int] with EfficientSubstep = r.stepper
    val s6: AnyStepper[Int] = r.stepper[Int, AnyStepper[Int]]
    val s7: AnyStepper[Int] with EfficientSubstep = r.stepper[Int, AnyStepper[Int]]
    // val s8: Stepper[Any] = r.stepper  // no StepperShape instance
    // val s9: Stepper[Long] = r.stepper // no StepperShape instance
  }

  @Test
  def stringStepper(): Unit = {
    val r = "millie"
    def s1 = r.stepper
    def s2: IntStepper = r.stepper
    def s3: IntStepper with EfficientSubstep = r.stepper
    def s4: Stepper[Int] = r.stepper
    def s5: Stepper[Int] with EfficientSubstep = r.stepper

    def cs1: IntStepper with EfficientSubstep = r.charStepper
    def ps1: IntStepper with EfficientSubstep = r.codePointStepper

    locally {
      import scala.jdk.StreamConverters.Ops._
      val rss = r.asJavaSeqStream
      (rss: IntStream).count()
      val rps = r.asJavaParStream
      (rps: IntStream).count()
      val rscs = r.asJavaSeqCharStream
      (rscs: IntStream).count()
      val rpcs = r.asJavaParCharStream
      (rpcs: IntStream).count()
      val rsps = r.asJavaSeqCodePointStream
      (rsps: IntStream).count()
      val rpps = r.asJavaParCodePointStream
      (rpps: IntStream).count()

      val isss = s3.asJavaSeqStream
      (isss: IntStream).count()
      val isps = s3.asJavaParStream
      (isps: IntStream).count()

      val siss = s5.asJavaSeqStream
      (siss: IntStream).count()
      val sips = s5.asJavaParStream
      (sips: IntStream).count()

      cs1.asJavaSeqStream.count()
      cs1.asJavaParStream.count()
      ps1.asJavaSeqStream.count()
      ps1.asJavaParStream.count()
    }
  }

  @Test
  def accumulatorStepper(): Unit = {
    val ia = Accumulator(1, 2, 3)
    val sa = Accumulator("a", "b", "c")

    locally {
      import scala.jdk.StreamConverters.Ops._

      val ias = ia.stepper
      (ias: IntStepper with EfficientSubstep).asJavaParStream.count()
      val iass = ia.asJavaSeqStream
      (iass: IntStream).count()
      val iaps = ia.asJavaParStream
      (iaps: IntStream).count()

      val sas = sa.stepper
      (sas: Stepper[String] with EfficientSubstep).asJavaParStream.count()
      val sass = sa.asJavaSeqStream
      (sass: Stream[String]).count()
      val saps = sa.asJavaParStream
      (saps: Stream[String]).count()
    }
  }
}
