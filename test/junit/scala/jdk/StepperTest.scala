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

import scala.collection.{AnyStepper, ClassTagIterableFactory, IntStepper, IterableFactory, MapFactory, SortedIterableFactory, SortedMapFactory, SpecificIterableFactory, Stepper, StepperShape}
import scala.collection.{concurrent => cc, immutable => ci, mutable => cm}
import scala.jdk.StreamConverters._
import scala.util.chaining._

@RunWith(classOf[JUnit4])
class StepperTest {
  val sizes = List(0, 1, 2, 3, 4, 7, 8, 15, 16, 17, 136, 2123)

  // NOTE: the ORDERED characteristic of a stepper does NOT mean the stepper yields elements in
  // the same order as they are stored in the underlying collection.
  // If a collection is ordered, the stepper is REQUIRED to yield elements in that order, just like
  // the collection's iterator.
  // The ORDERED flag on the stepper is `true` if there's *any* defined order in the stepper, and
  // that `trySplit` splits a prefix. See Javadocs of Spliterator.ORDERED. So an un-ordered
  // collection may have a stepper with the ORDERED flag, it's actually common, as the default
  // IterableOnce.stepper method has that flag.
  // For some stepper implementations, it makes sense not to be ORDERED, as this can allow more
  // efficient splitting.
  val hasSpecificNonOrderedStepper: Set[AnyRef] = Set(
    ci.HashSet, ci.HashMap, cm.HashMap
  )

  def hasOrderedFlag(companion: AnyRef) = !hasSpecificNonOrderedStepper(companion)

  // Boolean tells whether the collection's elements have an order.
  val factories: List[(IterableFactory[IterableOnce], Boolean)] =
    (ci.HashSet, false) ::
    List[IterableFactory[IterableOnce]](
      collection.Iterator,
      ci.ListSet, ci.LazyList, ci.List, ci.Vector,
      cm.ArrayBuffer, cm.Queue, cm.Stack, cm.ListBuffer, cm.ArrayDeque, cm.LinkedHashSet,
      scala.jdk.AnyAccumulator).map((_, true))

  val classTagFactories = List[ClassTagIterableFactory[IterableOnce]](
    ci.ArraySeq,
    cm.ArraySeq, cm.UnrolledBuffer)

  val sortedFactories = List[SortedIterableFactory[IterableOnce]](ci.TreeSet, cm.TreeSet)

  val mapFactories = List[(MapFactory[scala.collection.Map], Boolean)](
    (ci.HashMap, false), (ci.TreeSeqMap, true), (ci.ListMap, true), (ci.VectorMap, true),
    (cm.HashMap, false), (cm.LinkedHashMap, true),
    (cc.TrieMap, false))

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

  def sameElems(l: List[_], o: IterableOnce[_], testElemOrder: Boolean): Unit =
    if (testElemOrder)
      assertTrue(l sameElements o)
    else {
      val ls = l.toSet
      assertEquals(l.size, ls.size) // l is distinct
      assertEquals(ls, o.iterator.toSet)
    }

  def testStepper[T, U](anyStepper: => AnyStepper[T], specificStepper: => Stepper[U], l: List[T], size: Int, testElemOrder: Boolean, orderedFlag: Boolean): Unit = {
    def check(st: Stepper[_]): Boolean =
      st != null && {
        if ((st.characteristics & Spliterator.SIZED) != 0)
          assertEquals(st.estimateSize, size)
        else
          assertTrue(st.estimateSize >= size)
        assertEquals(orderedFlag, (st.characteristics & Spliterator.ORDERED) != 0)
        true
      }
    def test(st: => Stepper[_]): Unit =
      if (check(st)) {
        sameElems(l, st.iterator, testElemOrder)
        sameElems(l, splitAndCombine(st), testElemOrder)
        sameElems(l, spliteratorForEachElems(st), testElemOrder)
        sameElems(l, st.asJavaSeqStream.toScala(List), testElemOrder)
        sameElems(l, st.asJavaSeqStream.parallel.toScala(List), testElemOrder)
      }
    test(specificStepper)
    test(anyStepper)
  }

  def anyStepper[T](c: IterableOnce[T]): AnyStepper[T] = c.stepper

  @Test
  def iterableSteppers(): Unit =
    for (size <- sizes; (factory, elemsOrdered) <- factories) {
      val l = List.fill(size)(r.nextInt())
      def c = factory.from(l)
      testStepper(anyStepper(c), c.stepper: IntStepper, l, size, elemsOrdered, hasOrderedFlag(factory))
      val sl = l.map(_.toString)
      def sc = factory.from(sl)
      testStepper(anyStepper(sc), sc.stepper, sl, size, elemsOrdered, hasOrderedFlag(factory))
    }

  @Test
  def classTagIterableSteppers(): Unit = {
    for (size <- sizes; factory <- classTagFactories) {
      val l = List.fill(size)(r.nextInt())
      val c = factory.from(l)
      testStepper(anyStepper(c), c.stepper: IntStepper, l, size, testElemOrder = true, hasOrderedFlag(factory))
      val sl = l.map(_.toString)
      val sc = factory.from(sl)
      testStepper(anyStepper(sc), sc.stepper, sl, size, testElemOrder = true, hasOrderedFlag(factory))
    }
  }

  @Test
  def sortedIterableSteppers(): Unit = {
    for (size <- sizes; factory <- sortedFactories) {
      val l = List.fill(size)(r.nextInt()).distinct.sorted
      val c = factory.from(l)
      testStepper(anyStepper(c), c.stepper: IntStepper, l, size, testElemOrder = true, hasOrderedFlag(factory))
      val sl = l.map(_.toString).sorted
      val sc = factory.from(sl)
      testStepper(anyStepper(sc), sc.stepper, sl, size, testElemOrder = true, hasOrderedFlag(factory))
    }
  }

  @Test
  def specificIterableSteppers(): Unit =
    for (size <- sizes) {
      locally {
        def test(factory: SpecificIterableFactory[Int, IterableOnce[Int]], set: Boolean): Unit = {
          def next() = if (set) r.nextInt(100000) else r.nextInt()
          val l = List.fill(size)(next()).pipe(x => if (set) x.distinct.sorted else x)
          val c = factory.fromSpecific(l)
          testStepper(anyStepper(c), c.stepper: IntStepper, l, size, testElemOrder = true, hasOrderedFlag(factory))
        }
        test(ci.BitSet, set = true)
        test(cm.BitSet, set = true)
        test(scala.jdk.IntAccumulator, set = false)
      }
      locally {
        val l = List.fill(size)(r.nextInt().toChar)
        val c = ci.WrappedString.fromSpecific(l)
        testStepper(anyStepper(c), c.stepper, l, size, testElemOrder = true, orderedFlag = true)
      }
      locally {
        val l = List.fill(size)(r.nextLong())
        val c = scala.jdk.LongAccumulator.fromSpecific(l)
        testStepper(anyStepper(c), c.stepper, l, size, testElemOrder = true, orderedFlag = true)
      }
      locally {
        val l = List.fill(size)(r.nextDouble())
        val c = scala.jdk.DoubleAccumulator.fromSpecific(l)
        testStepper(anyStepper(c), c.stepper, l, size, testElemOrder = true, orderedFlag = true)
      }
    }

  def testMap[K, V](l: List[(K, V)], m: collection.Map[K, V], size: Int, testElemOrder: Boolean, orderedFlag: Boolean): Unit = {
    testStepper(anyStepper(m), m.stepper, l, size, testElemOrder, orderedFlag)

    val kl = l.map(_._1)
    val ks = m.keySet
    testStepper(anyStepper(ks), ks.stepper, kl, size, testElemOrder, orderedFlag = true) // keySet uses IteratorStepper, so has ordered flag

    def ki = m.keysIterator
    testStepper(anyStepper(ki), ki.stepper, kl, size, testElemOrder, orderedFlag = true)

    val vl = l.map(_._2)
    val vc = m.values
    testStepper(anyStepper(vc), vc.stepper, vl, size, testElemOrder = false, orderedFlag = true) // // values uses IteratorStepper, so has ordered flag

    def vi = m.valuesIterator
    testStepper(anyStepper(vi), vi.stepper, vl, size, testElemOrder = false, orderedFlag = true)
  }

  @Test
  def mapSteppers(): Unit = {
    for (size <- sizes; (factory, elemsOrdered) <- mapFactories) {
      val l = List.fill(size)(r.nextInt() -> r.nextInt()).distinctBy(_._1)
      testMap(l, factory.from(l), size, elemsOrdered, hasOrderedFlag(factory))

      val skl = l.map({case (k, v) => (k.toString, v)})
      testMap(skl, factory.from(skl), size, elemsOrdered, hasOrderedFlag(factory))

      val svl = l.map({case (k, v) => (k, v.toString)})
      testMap(svl, factory.from(svl), size, elemsOrdered, hasOrderedFlag(factory))
    }
  }

  @Test
  def sortedMapSteppers(): Unit =
    for (size <- sizes) {
      def test(factory: SortedMapFactory[collection.Map], isOrdered: Boolean) = {
        val l = List.fill(size)(r.nextInt() -> r.nextInt()).distinctBy(_._1).sortBy(_._1)
        testMap(l, factory.from(l), size, testElemOrder = isOrdered, hasOrderedFlag(factory))

        val skl = l.map { case (k, v) => (k.toString, v) }.sortBy(_._1)
        testMap(skl, factory.from(skl), size, testElemOrder = isOrdered, hasOrderedFlag(factory))

        val svl = l.map { case (k, v) => (k, v.toString) }.sortBy(_._1)
        testMap(svl, factory.from(svl), size, testElemOrder = isOrdered, hasOrderedFlag(factory))
      }
      test(ci.TreeMap, isOrdered = true)
      test(cm.TreeMap, isOrdered = true)
      test(cm.CollisionProofHashMap, isOrdered = false)
    }

  @Test
  def intMapSteppers(): Unit =
    for (size <- sizes) {
      val l = List.fill(size)(r.nextInt() -> r.nextInt()).distinctBy(_._1)
      testMap(l, ci.IntMap.from(l), size, testElemOrder = false, hasOrderedFlag(ci.IntMap))

      val svl = l.map({case (k, v) => (k, v.toString)})
      testMap(svl, ci.IntMap.from(svl), size, testElemOrder = false, hasOrderedFlag(ci.IntMap))
    }

  @Test
  def longMapSteppers(): Unit =
    for (size <- sizes) {
      type From[V] = IterableOnce[(Long, V)]
      def test[CC[V] <: collection.Map[Long, V]](factory: AnyRef, mki: From[Int] => CC[Int], mks: From[String] => CC[String]): Unit = {
        val l = List.fill(size)(r.nextLong() -> r.nextInt()).distinctBy(_._1)
        testMap(l, mki(l), size, testElemOrder = false, hasOrderedFlag(factory))

        val svl = l.map { case (k, v) => (k, v.toString) }
        testMap(svl, mks(svl), size, testElemOrder = false, hasOrderedFlag(factory))
      }
      test(ci.LongMap, ci.LongMap.from[Int](_), ci.LongMap.from[String](_))
      test(cm.LongMap, cm.LongMap.from[Int](_), cm.LongMap.from[String](_))
    }

  @Test
  @annotation.nowarn("cat=deprecation&origin=scala.collection.mutable.AnyRefMap")
  def anyRefMapSteppers(): Unit =
    for (size <- sizes) {
      val l = List.fill(size)(r.nextInt().toString -> r.nextInt()).distinctBy(_._1)
      testMap(l, cm.AnyRefMap.from(l), size, testElemOrder = false, hasOrderedFlag(cm.AnyRefMap))

      val svl = l.map { case (k, v) => (k, v.toString) }
      testMap(svl, cm.AnyRefMap.from(svl), size, testElemOrder = false, hasOrderedFlag(cm.AnyRefMap))
    }

  @Test
  def arraySteppers(): Unit = {
    for (size <- sizes) {
      val l = List.fill(size)(r.nextInt())
      val ia = Array.from(l)
      testStepper(anyStepper(ia), ia.stepper, l, size, testElemOrder = true, orderedFlag = true)
      val sa = ia.map(_.toString)
      testStepper(anyStepper(sa), sa.stepper, l.map(_.toString), size, testElemOrder = true, orderedFlag = true)
    }
  }

  @Test
  def stringStepper(): Unit = {
    for (size <- sizes) {
      val s = r.nextString(size)
      val chars = s.iterator.toList
      testStepper(null, s.charStepper, chars, size, testElemOrder = true, orderedFlag = true)
      val codePoints = s.codePoints().toScala(List)
      testStepper(null, s.codePointStepper, codePoints, size, testElemOrder = true, orderedFlag = true)
    }
  }

  // demonstrate beauty of stepper signature, see a574e3a
  class MyIterableOnce extends IterableOnce[Int] {
    private val data = Array(1, 2, 3)
    def iterator = data.iterator
    override def knownSize = data.length
    override def stepper[S <: Stepper[_]](implicit shape: StepperShape[Int, S]): S = data.stepper[S]
  }
}
