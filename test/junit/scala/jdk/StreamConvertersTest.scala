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

package scala.jdk

import java.util.stream.{DoubleStream, IntStream, LongStream, Stream => JStream}

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.jdk.StreamConverters._

@RunWith(classOf[JUnit4])
class StreamConvertersTest {
  def assertEq[A](a1: A, a2: A, s: String): Unit = { assertEquals(s, a1, a2) }  // Weird order normally!
  def assertEq[A](a1: A, a2: A): Unit = { assertEq(a1, a2, "not equal") }
  def assert(b: Boolean): Unit = { assertTrue(b) }
  def assert(b: Boolean, s: String): Unit = { assertTrue(s, b) }

  def arrayO(n: Int) = (1 to n).map(_.toString).toArray
  def arrayD(n: Int) = (1 to n).map(_.toDouble).toArray
  def arrayI(n: Int) = (1 to n).toArray
  def arrayL(n: Int) = (1 to n).map(_.toLong).toArray

  def newStream(n: Int) = java.util.Arrays.stream(arrayO(n))
  def newDoubleStream(n: Int) = java.util.Arrays.stream(arrayD(n))
  def newIntStream(n: Int) = java.util.Arrays.stream(arrayI(n))
  def newLongStream(n: Int) = java.util.Arrays.stream(arrayL(n))

  val ns = Vector(0, 1, 2, 12, 15, 16, 17, 31, 32, 33, 151, 1298, 7159)

  @Test
  def streamAccumulate(): Unit = {
    for (n <- ns) {
      val vecO = arrayO(n).toVector
      val accO = newStream(n).parallel.toScala(Accumulator)
      assertEq(vecO, newStream(n).toScala(Accumulator).to(Vector), s"stream $n to vector")
      assertEq(vecO, accO.to(Vector), s"stream $n to vector in parallel")
      assertEq(vecO, accO.toArray.toVector, s"stream $n to vector via array in parallel")
      assertEq(vecO, accO.iterator.toVector, s"stream $n to vector via iterator in parallel")
      assertEq(vecO, accO.toList.toVector, s"stream $n to vector via list in parallel")
      assert((0 until accO.size.toInt).forall(i => vecO(i) == accO(i)), s"stream $n indexed via accumulator")
      assert(accO.isInstanceOf[AnyAccumulator[_]], s"stream $n to generic accumulator")

      for (boxless <- Seq(false, true)) {
        val sbox = (if (boxless) "" else "(boxed)")
        val vecD = arrayD(n).toVector
        val accD =
          if (boxless) newDoubleStream(n).parallel.toScala(Accumulator)
          else newDoubleStream(n).boxed.parallel.toScala(Accumulator)
        assertEq(vecD, newDoubleStream(n).toScala(Accumulator).to(Vector), s"double stream $n to vector $sbox")
        assertEq(vecD, accD.to(Vector), s"double stream $n to vector in parallel $sbox")
        assertEq(vecD, accD.toArray.toVector, s"double stream $n to vector via array in parallel $sbox")
        assertEq(vecD, accD.iterator.toVector, s"double stream $n to vector via iterator in parallel $sbox")
        assertEq(vecD, accD.toList.toVector, s"double stream $n to vector via list in parallel $sbox")
        assert((0 until accD.size.toInt).forall(i => vecD(i) == accD(i)), s"double stream $n indexed via accumulator $sbox")
        assert(accD.isInstanceOf[DoubleAccumulator], s"double stream $n to generic accumulator $sbox")

        val vecI = arrayI(n).toVector
        val accI =
          if (boxless) newIntStream(n).parallel.toScala(Accumulator)
          else newIntStream(n).boxed.parallel.toScala(Accumulator)
        assertEq(vecI, newIntStream(n).toScala(Accumulator).to(Vector), s"int stream $n to vector $sbox")
        assertEq(vecI, accI.to(Vector), s"int stream $n to vector in parallel $sbox")
        assertEq(vecI, accI.toArray.toVector, s"int stream $n to vector via array in parallel $sbox")
        assertEq(vecI, accI.iterator.toVector, s"int stream $n to vector via iterator in parallel $sbox")
        assertEq(vecI, accI.toList.toVector, s"int stream $n to vector via list in parallel $sbox")
        assert((0 until accI.size.toInt).forall(i => vecI(i) == accI(i)), s"int stream $n indexed via accumulator $sbox")
        assert(accI.isInstanceOf[IntAccumulator], s"int stream $n to generic accumulator $sbox")

        val vecL = arrayL(n).toVector
        val accL =
          if (boxless) newLongStream(n).parallel.toScala(Accumulator)
          else newLongStream(n).boxed.parallel.toScala(Accumulator)
        assertEq(vecL, newLongStream(n).toScala(Accumulator).to(Vector), s"long stream $n to vector $sbox")
        assertEq(vecL, accL.to(Vector), s"long stream $n to vector in parallel $sbox")
        assertEq(vecL, accL.toArray.toVector, s"long stream $n to vector via array in parallel $sbox")
        assertEq(vecL, accL.iterator.toVector, s"long stream $n to vector via iterator in parallel $sbox")
        assertEq(vecL, accL.toList.toVector, s"long stream $n to vector via list in parallel $sbox")
        assert((0 until accL.size.toInt).forall(i => vecL(i) == accL(i)), s"long stream $n indexed via accumulator $sbox")
        assert(accL.isInstanceOf[LongAccumulator], s"long stream $n to generic accumulator $sbox")
      }
    }
  }

  @Test
  def streamToScala(): Unit = {
    for (n <- ns) {
      val vecO = arrayO(n).toVector
      assertEq(vecO, newStream(n).toScala(Vector))
      assertEq(vecO, newStream(n).parallel.toScala(Vector))

      val vecD = arrayD(n).toVector
      assertEq(vecD, newDoubleStream(n).toScala(Vector))
      assertEq(vecD, newDoubleStream(n).parallel.toScala(Vector))

      val vecI = arrayI(n).toVector
      assertEq(vecI, newIntStream(n).toScala(Vector))
      assertEq(vecI, newIntStream(n).parallel.toScala(Vector))

      val vecL = arrayL(n).toVector
      assertEq(vecL, newLongStream(n).toScala(Vector))
      assertEq(vecL, newLongStream(n).parallel.toScala(Vector))
    }
  }

  @Test
  def streamUnbox(): Unit = {
    assert(newDoubleStream(1).boxed.asJavaPrimitiveStream.isInstanceOf[DoubleStream])
    assert(newIntStream(1).boxed.asJavaPrimitiveStream.isInstanceOf[IntStream])
    assert(newLongStream(1).boxed.asJavaPrimitiveStream.isInstanceOf[LongStream])
  }

  import collection.mutable.{ArrayBuffer, ArraySeq}
  def abufO(n: Int) = { val ab = new ArrayBuffer[String]; arrayO(n).foreach(ab += _); ab }
  def abufD(n: Int) = { val ab = new ArrayBuffer[Double]; arrayD(n).foreach(ab += _); ab }
  def abufI(n: Int) = { val ab = new ArrayBuffer[Int]; arrayI(n).foreach(ab += _); ab }
  def abufL(n: Int) = { val ab = new ArrayBuffer[Long]; arrayL(n).foreach(ab += _); ab }
  def wrapO(n: Int): ArraySeq[String] = arrayO(n)
  def wrapD(n: Int): ArraySeq[Double] = arrayD(n)
  def wrapI(n: Int): ArraySeq[Int] = arrayI(n)
  def wrapL(n: Int): ArraySeq[Long] = arrayL(n)
  def vectO(n: Int) = arrayO(n).toVector
  def vectD(n: Int) = arrayD(n).toVector
  def vectI(n: Int) = arrayI(n).toVector
  def vectL(n: Int) = arrayL(n).toVector
  def genhset[A](aa: Array[A]) = { val hs = new collection.mutable.HashSet[A]; aa.foreach(hs += _); hs }
  def hsetO(n: Int) = genhset(arrayO(n))
  def hsetD(n: Int) = genhset(arrayD(n))
  def hsetI(n: Int) = genhset(arrayI(n))
  def hsetL(n: Int) = genhset(arrayL(n))

  @Test
  def scalaToStream(): Unit = {
    for (n <- ns) {
      val arrO = arrayO(n)
      val seqO = arrO.toSeq
      val abO = abufO(n)
      val wrO = wrapO(n)
      val vecO = vectO(n)
      val hsO = hsetO(n)
      // Seems like a lot of boilerplate, but we need it to test implicit resolution
      assertEq(seqO, seqO.asJavaSeqStream.toScala(Seq))
      assertEq(seqO, arrO.asJavaSeqStream.toScala(Seq))
      assertEq(seqO, arrO.asJavaParStream.toScala(Seq))
      assertEq(seqO, abO.asJavaSeqStream.toScala(Seq))
      assertEq(seqO, abO.asJavaParStream.toScala(Seq))
      assertEq(seqO, wrO.asJavaSeqStream.toScala(Seq))
      assertEq(seqO, wrO.asJavaParStream.toScala(Seq))
      assertEq(seqO, vecO.asJavaSeqStream.toScala(Seq))
      assertEq(seqO, vecO.asJavaParStream.toScala(Seq))
      assertEq(seqO, hsO.asJavaSeqStream.toScala(Seq).sortBy(_.toInt))
      assertEq(seqO, hsO.asJavaParStream.toScala(Seq).sortBy(_.toInt))

      val arrD = arrayD(n)
      val seqD = arrD.toSeq
      val abD = abufD(n)
      val wrD = wrapD(n)
      val vecD = vectD(n)
      val hsD = hsetD(n)
      assertEq(seqD, seqD.asJavaSeqStream.toScala(Seq))
      assertEq(seqD, arrD.asJavaSeqStream.toScala(Seq))
      assertEq(seqD, arrD.asJavaParStream.toScala(Seq))
      assert(arrD.asJavaSeqStream.isInstanceOf[DoubleStream])
      assert(arrD.asJavaParStream.isInstanceOf[DoubleStream])
      assertEq(seqD, abD.asJavaSeqStream.toScala(Seq))
      assertEq(seqD, abD.asJavaParStream.toScala(Seq))
      assert(abD.asJavaSeqStream.isInstanceOf[DoubleStream])
      assert(abD.asJavaParStream.isInstanceOf[DoubleStream])
      assertEq(seqD, wrD.asJavaSeqStream.toScala(Seq))
      assertEq(seqD, wrD.asJavaParStream.toScala(Seq))
      assert(wrD.asJavaSeqStream.isInstanceOf[DoubleStream])
      assert(wrD.asJavaParStream.isInstanceOf[DoubleStream])
      assertEq(seqD, vecD.asJavaSeqStream.toScala(Seq))
      assertEq(seqD, vecD.asJavaParStream.toScala(Seq))
      assert(vecD.asJavaSeqStream.isInstanceOf[DoubleStream])
      assert(vecD.asJavaParStream.isInstanceOf[DoubleStream])
      assertEq(seqD, hsD.asJavaSeqStream.toScala(Seq).sorted)
      assertEq(seqD, hsD.asJavaParStream.toScala(Seq).sorted)
      assert(hsD.asJavaSeqStream.isInstanceOf[DoubleStream])
      assert(hsD.asJavaParStream.isInstanceOf[DoubleStream])

      val arrI = arrayI(n)
      val seqI = arrI.toSeq
      val abI = abufI(n)
      val wrI = wrapI(n)
      val vecI = vectI(n)
      val hsI = hsetI(n)
      assertEq(seqI, seqI.asJavaSeqStream.toScala(Seq))
      assertEq(seqI, arrI.asJavaSeqStream.toScala(Seq))
      assertEq(seqI, arrI.asJavaParStream.toScala(Seq))
      assert(arrI.asJavaSeqStream.isInstanceOf[IntStream])
      assert(arrI.asJavaParStream.isInstanceOf[IntStream])
      assertEq(seqI, abI.asJavaSeqStream.toScala(Seq))
      assertEq(seqI, abI.asJavaParStream.toScala(Seq))
      assert(abI.asJavaSeqStream.isInstanceOf[IntStream])
      assert(abI.asJavaParStream.isInstanceOf[IntStream])
      assertEq(seqI, wrI.asJavaSeqStream.toScala(Seq))
      assertEq(seqI, wrI.asJavaParStream.toScala(Seq))
      assert(wrI.asJavaSeqStream.isInstanceOf[IntStream])
      assert(wrI.asJavaParStream.isInstanceOf[IntStream])
      assertEq(seqI, vecI.asJavaSeqStream.toScala(Seq))
      assertEq(seqI, vecI.asJavaParStream.toScala(Seq))
      assert(vecI.asJavaSeqStream.isInstanceOf[IntStream])
      assert(vecI.asJavaParStream.isInstanceOf[IntStream])
      assertEq(seqI, hsI.asJavaSeqStream.toScala(Seq).sorted)
      assertEq(seqI, hsI.asJavaParStream.toScala(Seq).sorted)
      assert(hsI.asJavaSeqStream.isInstanceOf[IntStream])
      assert(hsI.asJavaParStream.isInstanceOf[IntStream])

      val arrL = arrayL(n)
      val seqL = arrL.toSeq
      val abL = abufL(n)
      val wrL = wrapL(n)
      val vecL = vectL(n)
      val hsL = hsetL(n)
      assertEq(seqL, seqL.asJavaSeqStream.toScala(Seq))
      assertEq(seqL, arrL.asJavaSeqStream.toScala(Seq))
      assertEq(seqL, arrL.asJavaParStream.toScala(Seq))
      assert(arrL.asJavaSeqStream.isInstanceOf[LongStream])
      assert(arrL.asJavaParStream.isInstanceOf[LongStream])
      assertEq(seqL, abL.asJavaSeqStream.toScala(Seq))
      assertEq(seqL, abL.asJavaParStream.toScala(Seq))
      assert(abL.asJavaSeqStream.isInstanceOf[LongStream])
      assert(abL.asJavaParStream.isInstanceOf[LongStream])
      assertEq(seqD, wrD.asJavaSeqStream.toScala(Seq))
      assertEq(seqD, wrD.asJavaParStream.toScala(Seq))
      assert(wrL.asJavaSeqStream.isInstanceOf[LongStream])
      assert(wrL.asJavaParStream.isInstanceOf[LongStream])
      assertEq(seqD, wrD.asJavaSeqStream.toScala(Seq))
      assertEq(seqD, wrD.asJavaParStream.toScala(Seq))
      assert(vecL.asJavaSeqStream.isInstanceOf[LongStream])
      assert(vecL.asJavaParStream.isInstanceOf[LongStream])
      assertEq(seqL, hsL.asJavaSeqStream.toScala(Seq).sorted)
      assertEq(seqL, hsL.asJavaParStream.toScala(Seq).sorted)
      assert(hsL.asJavaSeqStream.isInstanceOf[LongStream])
      assert(hsL.asJavaParStream.isInstanceOf[LongStream])
    }
  }

  @Test
  def primitiveStreamTypes(): Unit = {
    // Unboxed native + widening Steppers available:
    assertEquals(Vector[Int](1, 2, 3), (Array[Int](1, 2, 3).asJavaSeqStream: IntStream).toScala(Vector))
    assertEquals(Vector[Short](1.toShort, 2.toShort, 3.toShort), (Array[Short](1.toShort, 2.toShort, 3.toShort).asJavaSeqStream: IntStream).toScala(Vector))
    assertEquals(Vector[String]("a", "b"), (Array[String]("a", "b").asJavaSeqStream: JStream[String]).toScala(Vector))

    // Boxed collections, widening via boxed AnySteppers:
    assertEquals(Vector[Int](1, 2, 3), (Vector[Int](1, 2, 3).asJavaSeqStream: IntStream).toScala(Vector))
    assertEquals(Vector[Short](1.toShort, 2.toShort, 3.toShort), (Vector[Short](1.toShort, 2.toShort, 3.toShort).asJavaSeqStream: IntStream).toScala(Vector))
    assertEquals(Vector[String]("a", "b"), (Vector[String]("a", "b").asJavaSeqStream: JStream[String]).toScala(Vector))
  }
}
