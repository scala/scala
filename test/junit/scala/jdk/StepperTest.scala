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

import java.util.Spliterator

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.collection.{AnyStepper, ClassTagIterableFactory, IntStepper, IterableFactory, MapFactory, SortedIterableFactory, SortedMapFactory, SpecificIterableFactory, Stepper, concurrent => cc, immutable => ci, mutable => cm}
import scala.jdk.StreamConverters.Ops._
import scala.util.chaining._


@RunWith(classOf[JUnit4])
class StepperTest {
  val sizes = List(0, 1, 2, 3, 4, 7, 8, 15, 16, 17, 136, 2123)

  val factories: List[(IterableFactory[IterableOnce], Boolean)] =
    (ci.HashSet, false) ::
    List[IterableFactory[IterableOnce]](
      collection.Iterator,
      ci.ListSet, ci.LazyList, ci.List, ci.Vector,
      cm.ArrayBuffer, cm.Queue, cm.Stack, cm.ListBuffer, cm.ArrayDeque,
      scala.jdk.AnyAccumulator).map((_, true))

  val classTagFactories = List[ClassTagIterableFactory[IterableOnce]](
    ci.ArraySeq,
    cm.ArraySeq, cm.UnrolledBuffer)

  val sortedFactories = List[SortedIterableFactory[IterableOnce]](ci.TreeSet, cm.TreeSet)

  val mapFactories = List[(MapFactory[scala.collection.Map], Boolean)](
    (ci.HashMap, false), (ci.TreeSeqMap, true), (ci.ListMap, true), (ci.VectorMap, true),
    (cm.HashMap, false), (cm.LinkedHashMap, false), // TODO report issue: LinkedHashMap should be ordered
    (cc.TrieMap, false)) // TODO: report issue: TrieMap says its Stepper is ordered

  val r = new scala.util.Random(3123)

  def splitAndCombine[T](a: Stepper[T]): Iterator[T] = {
    val b1 = Option(a.trySplit())
    val b2 = Some(a)

    val c1 = b1.map(x => Option(x.trySplit()))
    val c2 = Some(b1)
    val c3 = b2.map(x => Option(x.trySplit()))
    val c4 = Some(b2)

    val steppers = List(c1, c2, c3, c4).flatten.flatten
    steppers.map(_.iterator).foldLeft(Iterator.empty[T])(_ concat _)
  }

  def spliteratorForEachElems[T](a: Stepper[T]): List[T] = {
    val b = List.newBuilder[T]
    a.spliterator.asInstanceOf[Spliterator[T]].forEachRemaining(b.addOne)
    b.result()
  }

  def sameElems[T](l: List[T], o: IterableOnce[T], ordered: Boolean): Unit = {
    if (ordered)
      assertTrue(l sameElements o)
    else
      assertEquals(l.toSet, o.iterator.toSet)
  }

  def testStepper[T, U](anyStepper: => AnyStepper[T], specificStepper: => Stepper[U], l: List[T], size: Int, ordered: Boolean = true, testOrderedFlag: Boolean = true): Unit = {
    for (st <- List(() => specificStepper, () => anyStepper) if st() != null) {
      if ((st().characteristics & Spliterator.SIZED) != 0)
        assertEquals(st().estimateSize, size)
      else
        assertTrue(st().estimateSize >= size)

      if (testOrderedFlag)
        assertEquals(ordered, (st().characteristics & Spliterator.ORDERED) != 0)

      sameElems(l, st().iterator, ordered)
      sameElems(l, splitAndCombine(st()), ordered)
      sameElems(l, spliteratorForEachElems(st()), ordered)
      sameElems(l, st().asJavaSeqStream.toScala(List), ordered)
      sameElems(l, st().asJavaSeqStream.parallel.toScala(List), ordered)
    }
  }

  def anyStepper[T](c: IterableOnce[T]): AnyStepper[T] = c.stepper

  @Test
  def iterableSteppers(): Unit = {
    for (size <- sizes; (factory, ordered) <- factories) {
      val l = List.fill(size)(r.nextInt())
      def c = factory.from(l)
      testStepper(anyStepper(c), c.stepper: IntStepper, l, size, ordered)
      val sl = l.map(_.toString)
      def sc = factory.from(sl)
      testStepper(anyStepper(sc), sc.stepper, sl, size, ordered)
    }
  }

  @Test
  def classTagIterableSteppers(): Unit = {
    for (size <- sizes; factory <- classTagFactories) {
      val l = List.fill(size)(r.nextInt())
      val c = factory.from(l)
      testStepper(anyStepper(c), c.stepper: IntStepper, l, size)
      val sl = l.map(_.toString)
      val sc = factory.from(sl)
      testStepper(anyStepper(sc), sc.stepper, sl, size)
    }
  }

  @Test
  def sortedIterableSteppers(): Unit = {
    for (size <- sizes; factory <- sortedFactories) {
      val l = List.fill(size)(r.nextInt()).distinct.sorted
      val c = factory.from(l)
      testStepper(anyStepper(c), c.stepper: IntStepper, l, size)
      val sl = l.map(_.toString).sorted
      val sc = factory.from(sl)
      testStepper(anyStepper(sc), sc.stepper, sl, size)
    }
  }

  @Test
  def specificIterableSteppers(): Unit = {
    for (size <- sizes) {
      for ((factory, set) <- List[(SpecificIterableFactory[Int, IterableOnce[Int]], Boolean)](ci.BitSet -> true, cm.BitSet -> true, scala.jdk.IntAccumulator -> false)) {
        val l = List.fill(size)(if(set) r.nextInt(100000) else r.nextInt()).pipe(x => if (set) x.distinct.sorted else x)
        val c = factory.fromSpecific(l)
        testStepper(anyStepper(c), c.stepper: IntStepper, l, size)
      }
      locally {
        val l = List.fill(size)(r.nextInt().toChar)
        val c = ci.WrappedString.fromSpecific(l)
        testStepper(anyStepper(c), c.stepper, l, size)
      }
      locally {
        val l = List.fill(size)(r.nextLong())
        val c = scala.jdk.LongAccumulator.fromSpecific(l)
        testStepper(anyStepper(c), c.stepper, l, size)
      }
      locally {
        val l = List.fill(size)(r.nextDouble())
        val c = scala.jdk.DoubleAccumulator.fromSpecific(l)
        testStepper(anyStepper(c), c.stepper, l, size)
      }
    }
  }

  def testMap(l: List[(Int, Int)], m: collection.Map[Int, Int], ordered: Boolean, size: Int, factory: AnyRef): Unit = {
    val testOrd = factory != cm.LinkedHashMap && factory != cc.TrieMap
    testStepper(anyStepper(m), m.stepper, l, size, ordered, testOrd)

    // TODO: keySet steppers say they're ordered.. (IteratorStepper)

    val kl = l.map(_._1)
    val ks = m.keySet
    testStepper(anyStepper(ks), ks.stepper, kl, size, ordered, testOrderedFlag = false)

    def ki = m.keysIterator
    testStepper(anyStepper(ki), ki.stepper, kl, size, ordered, testOrderedFlag = false)

    val skl = kl.map(_.toString)
    val sks = m.map({case (k, v) => (k.toString, v)}).keySet
    testStepper(anyStepper(sks), sks.stepper, skl, size, ordered && factory != cm.TreeMap && factory != ci.TreeMap, testOrderedFlag = false)

    def ski = m.map({case (k, v) => (k.toString, v)}).keysIterator
    testStepper(anyStepper(ski), ski.stepper, skl, size, ordered && factory != cm.TreeMap && factory != ci.TreeMap, testOrderedFlag = false)

    val vl = l.map(_._2)
    val vc = m.values
    testStepper(anyStepper(vc), vc.stepper, vl, size, ordered = false, testOrderedFlag = false)

    def vi = m.valuesIterator
    testStepper(anyStepper(vi), vi.stepper, vl, size, ordered = false, testOrderedFlag = false)

    val svl = vl.map(_.toString)
    val svc = m.map({case (k, v) => (k, v.toString)}).values
    testStepper(anyStepper(svc), svc.stepper, svl, size, ordered = false, testOrderedFlag = false)

    def svi = m.map({case (k, v) => (k, v.toString)}).valuesIterator
    testStepper(anyStepper(svi), svi.stepper, svl, size, ordered = false, testOrderedFlag = false)
  }

  @Test
  def mapSteppers(): Unit = {
    for (size <- sizes; (factory, ordered) <- mapFactories) {
      val l = List.fill(size)(r.nextInt() -> r.nextInt()).distinctBy(_._1)
      val m = factory.from(l)
      testMap(l, m, ordered, size, factory)
    }
  }

  @Test
  def sortedMapSteppers(): Unit = {
    for (size <- sizes; factory <- List[SortedMapFactory[collection.Map]](ci.TreeMap, cm.TreeMap)) {
      val l = List.fill(size)(r.nextInt() -> r.nextInt()).distinctBy(_._1).sortBy(_._1)
      val m = factory.from(l)
      testMap(l, m, ordered = true, size, factory)
    }
  }

  @Test
  def intMapSteppers(): Unit = {
    for (size <- sizes) {
      val l = List.fill(size)(r.nextInt() -> r.nextInt()).distinctBy(_._1)
      val m = ci.IntMap.from(l)

      testStepper(anyStepper(m), m.stepper, l, size, ordered = false, testOrderedFlag = false)

      val kl = l.map(_._1)
      val ks = m.keySet
      testStepper(anyStepper(ks), ks.stepper, kl, size, ordered = false, testOrderedFlag = false)

      def ki = m.keysIterator
      testStepper(anyStepper(ki), ki.stepper, kl, size, ordered = false, testOrderedFlag = false)

      val vl = l.map(_._2)
      val vc = m.values
      testStepper(anyStepper(vc), vc.stepper, vl, size, ordered = false, testOrderedFlag = false)

      def vi = m.valuesIterator
      testStepper(anyStepper(vi), vi.stepper, vl, size, ordered = false, testOrderedFlag = false)

      val svl = vl.map(_.toString)
      val svc = m.map({case (k, v) => (k, v.toString)}).values
      testStepper(anyStepper(svc), svc.stepper, svl, size, ordered = false, testOrderedFlag = false)

      def svi = m.map({case (k, v) => (k, v.toString)}).valuesIterator
      testStepper(anyStepper(svi), svi.stepper, svl, size, ordered = false, testOrderedFlag = false)
    }
  }

  @Test
  def longMapSteppers(): Unit = {
    for (size <- sizes; factory <- List(ci.LongMap.from[Int] _, cm.LongMap.from[Int] _)) {
      val l = List.fill(size)(r.nextLong() -> r.nextInt()).distinctBy(_._1)
      val m = factory(l)

      testStepper(anyStepper(m), m.stepper, l, size, ordered = false, testOrderedFlag = false)

      val kl = l.map(_._1)
      val ks = m.keySet
      testStepper(anyStepper(ks), ks.stepper, kl, size, ordered = false, testOrderedFlag = false)

      def ki = m.keysIterator
      testStepper(anyStepper(ki), ki.stepper, kl, size, ordered = false, testOrderedFlag = false)

      val vl = l.map(_._2)
      val vc = m.values
      testStepper(anyStepper(vc), vc.stepper, vl, size, ordered = false, testOrderedFlag = false)

      def vi = m.valuesIterator
      testStepper(anyStepper(vi), vi.stepper, vl, size, ordered = false, testOrderedFlag = false)

      val svl = vl.map(_.toString)
      val svc = m.map({case (k, v) => (k, v.toString)}).values
      testStepper(anyStepper(svc), svc.stepper, svl, size, ordered = false, testOrderedFlag = false)

      def svi = m.map({case (k, v) => (k, v.toString)}).valuesIterator
      testStepper(anyStepper(svi), svi.stepper, svl, size, ordered = false, testOrderedFlag = false)
    }
  }

  @Test
  def anyRefMapSteppers(): Unit = {
    for (size <- sizes) {
      val l = List.fill(size)(r.nextInt().toString -> r.nextInt()).distinctBy(_._1)
      val m = cm.AnyRefMap.from(l)

      testStepper(anyStepper(m), m.stepper, l, size, ordered = false, testOrderedFlag = false)

      val kl = l.map(_._1)
      val ks = m.keySet
      testStepper(anyStepper(ks), ks.stepper, kl, size, ordered = false, testOrderedFlag = false)

      def ki = m.keysIterator
      testStepper(anyStepper(ki), ki.stepper, kl, size, ordered = false, testOrderedFlag = false)

      val vl = l.map(_._2)
      val vc = m.values
      testStepper(anyStepper(vc), vc.stepper, vl, size, ordered = false, testOrderedFlag = false)

      def vi = m.valuesIterator
      testStepper(anyStepper(vi), vi.stepper, vl, size, ordered = false, testOrderedFlag = false)

      val svl = vl.map(_.toString)
      val svc = m.map({case (k, v) => (k, v.toString)}).values
      testStepper(anyStepper(svc), svc.stepper, svl, size, ordered = false, testOrderedFlag = false)

      def svi = m.map({case (k, v) => (k, v.toString)}).valuesIterator
      testStepper(anyStepper(svi), svi.stepper, svl, size, ordered = false, testOrderedFlag = false)
    }
  }

  @Test
  def arraySteppers(): Unit = {
    for (size <- sizes) {
      val l = List.fill(size)(r.nextInt())
      val ia = Array.from(l)
      testStepper(anyStepper(ia), ia.stepper, l, size)
      val sa = ia.map(_.toString)
      testStepper(anyStepper(sa), sa.stepper, l.map(_.toString), size)
    }
  }

  @Test
  def stringStepper(): Unit = {
    for (size <- sizes) {
      val s = r.nextString(size)
      val chars = s.iterator.toList
      testStepper(null, s.charStepper, chars, size)
      val codePoints = s.codePoints().toScala(List)
      testStepper(null, s.codePointStepper, codePoints, size)
    }
  }
}
