/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection

import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}
import java.util.{PrimitiveIterator, Spliterator, Iterator => JIterator}
import java.{lang => jl}

import scala.collection.Stepper.EfficientSplit

/** Steppers exist to enable creating Java streams over Scala collections, see
  * [[scala.jdk.StreamConverters]]. Besides that use case, they allow iterating over collections
  * holding unboxed primitives (e.g., `Array[Int]`) without boxing the elements.
  *
  * Steppers have an iterator-like interface with methods `hasStep` and `nextStep()`. The difference
  * to iterators - and the reason `Stepper` is not a subtype of `Iterator` - is that there are
  * hand-specialized variants of `Stepper` for `Int`, `Long` and `Double` ([[IntStepper]], etc.).
  * These enable iterating over collections holding unboxed primitives (e.g., Arrays,
  * [[scala.jdk.Accumulator]]s) without boxing the elements.
  *
  * The selection of primitive types (`Int`, `Long` and `Double`) matches the hand-specialized
  * variants of Java Streams ([[java.util.stream.Stream]], [[java.util.stream.IntStream]], etc.)
  * and the corresponding Java Spliterators ([[java.util.Spliterator]], [[java.util.Spliterator.OfInt]], etc.).
  *
  * Steppers can be converted to Scala Iterators, Java Iterators and Java Spliterators. Primitive
  * Steppers are converted to the corresponding primitive Java Iterators and Spliterators.
  *
  * @tparam A the element type of the Stepper
  */
trait Stepper[@specialized(Double, Int, Long) +A] {
  /** Check if there's an element available. */
  def hasStep: Boolean

  /** Return the next element and advance the stepper */
  def nextStep(): A

  /** Split this stepper, if applicable. The elements of the current Stepper are split up between
    * the resulting Stepper and the current stepper.
    *
    * May return `null`, in which case the current Stepper yields the same elements as before.
    *
    * See method `trySplit` in [[java.util.Spliterator]].
    */
  def trySplit(): Stepper[A]

  /** Returns an estimate of the number of elements of this Stepper, or [[Long.MaxValue]]. See
    * method `estimateSize` in [[java.util.Spliterator]].
    */
  def estimateSize: Long

  /** Returns a set of characteristics of this Stepper and its elements. See method
    * `characteristics` in [[java.util.Spliterator]].
    */
  def characteristics: Int

  /** Returns a [[java.util.Spliterator]] corresponding to this Stepper.
    *
    * Note that the return type is `Spliterator[_]` instead of `Spliterator[A]` to allow returning
    * a [[java.util.Spliterator.OfInt]] (which is a `Spliterator[Integer]`) in the subclass [[IntStepper]]
    * (which is a `Stepper[Int]`).
    */
  def spliterator[B >: A]: Spliterator[_]

  /** Returns a Java [[java.util.Iterator]] corresponding to this Stepper.
    *
    * Note that the return type is `Iterator[_]` instead of `Iterator[A]` to allow returning
    * a [[java.util.PrimitiveIterator.OfInt]] (which is a `Iterator[Integer]`) in the subclass
    * [[IntStepper]] (which is a `Stepper[Int]`).
    */
  def javaIterator[B >: A]: JIterator[_]

  /** Returns an [[Iterator]] corresponding to this Stepper. Note that Iterators corresponding to
    * primitive Steppers box the elements.
    */
  def iterator: Iterator[A] = new AbstractIterator[A] {
    def hasNext: Boolean = hasStep
    def next(): A = nextStep()
  }
}

object Stepper {
  /** A marker trait that indicates that a `Stepper` can call `trySplit` with at worst O(log N) time
    * and space complexity, and that the division is likely to be reasonably even. Steppers marked
    * with `EfficientSplit` can be converted to parallel streams with the `asJavaParStream` method
    * defined in [[scala.jdk.StreamConverters]].
    */
  trait EfficientSplit

  private[collection] final def throwNSEE(): Nothing = throw new NoSuchElementException("Empty Stepper")

  /* These adapter classes can wrap an AnyStepper of a numeric type into a possibly widened primitive Stepper type.
   * This provides a basis for more efficient stream processing on unboxed values provided that the original source
   * of the data is boxed. In other cases native implementations of the primitive stepper types should be provided
   * (see for example IntArrayStepper and WidenedByteArrayStepper). */

  private[collection] class UnboxingDoubleStepper(st: AnyStepper[Double]) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): DoubleStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingDoubleStepper(s)
    }
  }

  private[collection] class UnboxingIntStepper(st: AnyStepper[Int]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingIntStepper(s)
    }
  }

  private[collection] class UnboxingLongStepper(st: AnyStepper[Long]) extends LongStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): LongStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingLongStepper(s)
    }
  }

  private[collection] class UnboxingByteStepper(st: AnyStepper[Byte]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingByteStepper(s)
    }
  }

  private[collection] class UnboxingCharStepper(st: AnyStepper[Char]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingCharStepper(s)
    }
  }

  private[collection] class UnboxingShortStepper(st: AnyStepper[Short]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingShortStepper(s)
    }
  }

  private[collection] class UnboxingFloatStepper(st: AnyStepper[Float]) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): DoubleStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingFloatStepper(s)
    }
  }
}

/** A Stepper for arbitrary element types. See [[Stepper]]. */
trait AnyStepper[+A] extends Stepper[A] {
  def trySplit(): AnyStepper[A]

  def spliterator[B >: A]: Spliterator[B] = new AnyStepper.AnyStepperSpliterator(this)

  def javaIterator[B >: A]: JIterator[B] = new JIterator[B] {
    def hasNext: Boolean = hasStep
    def next(): B = nextStep()
  }
}

object AnyStepper {
  class AnyStepperSpliterator[A](s: AnyStepper[A]) extends Spliterator[A] {
    def tryAdvance(c: Consumer[_ >: A]): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    def trySplit(): Spliterator[A] = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: A]): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
  }

  def ofSeqDoubleStepper(st: DoubleStepper): AnyStepper[Double] = new BoxedDoubleStepper(st)
  def ofParDoubleStepper(st: DoubleStepper with EfficientSplit): AnyStepper[Double] with EfficientSplit = new BoxedDoubleStepper(st) with EfficientSplit

  def ofSeqIntStepper(st: IntStepper): AnyStepper[Int] = new BoxedIntStepper(st)
  def ofParIntStepper(st: IntStepper with EfficientSplit): AnyStepper[Int] with EfficientSplit = new BoxedIntStepper(st) with EfficientSplit

  def ofSeqLongStepper(st: LongStepper): AnyStepper[Long] = new BoxedLongStepper(st)
  def ofParLongStepper(st: LongStepper with EfficientSplit): AnyStepper[Long] with EfficientSplit = new BoxedLongStepper(st) with EfficientSplit

  private[collection] class BoxedDoubleStepper(st: DoubleStepper) extends AnyStepper[Double] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): AnyStepper[Double] = {
      val s = st.trySplit()
      if (s == null) null else new BoxedDoubleStepper(s)
    }
  }

  private[collection] class BoxedIntStepper(st: IntStepper) extends AnyStepper[Int] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): AnyStepper[Int] = {
      val s = st.trySplit()
      if (s == null) null else new BoxedIntStepper(s)
    }
  }

  private[collection] class BoxedLongStepper(st: LongStepper) extends AnyStepper[Long] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    def estimateSize: Long = st.estimateSize
    def characteristics: Int = st.characteristics
    def trySplit(): AnyStepper[Long] = {
      val s = st.trySplit()
      if (s == null) null else new BoxedLongStepper(s)
    }
  }
}

/** A Stepper for Ints. See [[Stepper]]. */
trait IntStepper extends Stepper[Int] {
  def trySplit(): IntStepper

  def spliterator[B >: Int]: Spliterator.OfInt = new IntStepper.IntStepperSpliterator(this)

  def javaIterator[B >: Int]: PrimitiveIterator.OfInt = new PrimitiveIterator.OfInt {
    def hasNext: Boolean = hasStep
    def nextInt(): Int = nextStep()
  }
}
object IntStepper {
  class IntStepperSpliterator(s: IntStepper) extends Spliterator.OfInt {
    def tryAdvance(c: IntConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    override def tryAdvance(c: Consumer[_ >: jl.Integer]): Boolean = (c: AnyRef) match {
      case ic: IntConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(jl.Integer.valueOf(s.nextStep())); true } else false
    }
    // override required for dotty#6152
    override def trySplit(): Spliterator.OfInt = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: IntConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: jl.Integer]): Unit = (c: AnyRef) match {
      case ic: IntConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Integer.valueOf(s.nextStep())) }
    }
  }
}

/** A Stepper for Doubles. See [[Stepper]]. */
trait DoubleStepper extends Stepper[Double] {
  def trySplit(): DoubleStepper

  def spliterator[B >: Double]: Spliterator.OfDouble = new DoubleStepper.DoubleStepperSpliterator(this)

  def javaIterator[B >: Double]: PrimitiveIterator.OfDouble = new PrimitiveIterator.OfDouble {
    def hasNext: Boolean = hasStep
    def nextDouble(): Double = nextStep()
  }
}

object DoubleStepper {
  class DoubleStepperSpliterator(s: DoubleStepper) extends Spliterator.OfDouble {
    def tryAdvance(c: DoubleConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    override def tryAdvance(c: Consumer[_ >: jl.Double]): Boolean = (c: AnyRef) match {
      case ic: DoubleConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(java.lang.Double.valueOf(s.nextStep())); true } else false
    }
    // override required for dotty#6152
    override def trySplit(): Spliterator.OfDouble = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: DoubleConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: jl.Double]): Unit = (c: AnyRef) match {
      case ic: DoubleConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Double.valueOf(s.nextStep())) }
    }
  }
}

/** A Stepper for Longs. See [[Stepper]]. */
trait LongStepper extends Stepper[Long] {
  def trySplit(): LongStepper

  def spliterator[B >: Long]: Spliterator.OfLong = new LongStepper.LongStepperSpliterator(this)

  def javaIterator[B >: Long]: PrimitiveIterator.OfLong = new PrimitiveIterator.OfLong {
    def hasNext: Boolean = hasStep
    def nextLong(): Long = nextStep()
  }
}

object LongStepper {
  class LongStepperSpliterator(s: LongStepper) extends Spliterator.OfLong {
    def tryAdvance(c: LongConsumer): Boolean =
      if (s.hasStep) { c.accept(s.nextStep()); true } else false
    // Override for efficiency: don't wrap the function and call the `tryAdvance` overload
    override def tryAdvance(c: Consumer[_ >: jl.Long]): Boolean = (c: AnyRef) match {
      case ic: LongConsumer => tryAdvance(ic)
      case _ => if (s.hasStep) { c.accept(java.lang.Long.valueOf(s.nextStep())); true } else false
    }
    // override required for dotty#6152
    override def trySplit(): Spliterator.OfLong = {
      val sp = s.trySplit()
      if (sp == null) null else sp.spliterator
    }
    def estimateSize(): Long = s.estimateSize
    def characteristics(): Int = s.characteristics
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: LongConsumer): Unit =
      while (s.hasStep) { c.accept(s.nextStep()) }
    // Override for efficiency: implement with hasStep / nextStep instead of tryAdvance
    override def forEachRemaining(c: Consumer[_ >: jl.Long]): Unit = (c: AnyRef) match {
      case ic: LongConsumer => forEachRemaining(ic)
      case _ => while (s.hasStep) { c.accept(jl.Long.valueOf(s.nextStep())) }
    }
  }
}
