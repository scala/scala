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

package scala.collection

import java.util.function.{Consumer, DoubleConsumer, IntConsumer, LongConsumer}
import java.util.{PrimitiveIterator, Spliterator, Iterator => JIterator}
import java.{lang => jl}

import scala.collection.Stepper.EfficientSplit

trait Stepper[@specialized(Double, Int, Long) A] {
  def hasStep: Boolean

  def nextStep(): A

  // may return null
  private[collection] def trySplit(): Stepper[A]

  private[collection] def estimateSize: Long

  private[collection] def characteristics: Int

  // Not Spliterator[A] because when A=Int, we refine the type to Spliterator.OfInt, which is a
  // subtype of Spliterator[Integer]. Could use a shape typeclass implicit argument to express it.
  def spliterator: Spliterator[_]

  def javaIterator: JIterator[_]

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

  /* These adapter classes can wrap an AnyStepper of anumeric type into a possibly widened primitive Stepper type.
   * This provides a basis for more efficient stream processing on unboxed values provided that the original source
   * of the data is boxed. In other cases native implementations of the primitive stepper types should be provided
   * (see for example StepsIntArray and StepsWidenedByteArray). */

  private[collection] class UnboxingDoubleStepper(st: AnyStepper[Double]) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): DoubleStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingDoubleStepper(s)
    }
  }

  private[collection] class UnboxingIntStepper(st: AnyStepper[Int]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingIntStepper(s)
    }
  }

  private[collection] class UnboxingLongStepper(st: AnyStepper[Long]) extends LongStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): LongStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingLongStepper(s)
    }
  }

  private[collection] class UnboxingByteStepper(st: AnyStepper[Byte]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingByteStepper(s)
    }
  }

  private[collection] class UnboxingCharStepper(st: AnyStepper[Char]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingCharStepper(s)
    }
  }

  private[collection] class UnboxingShortStepper(st: AnyStepper[Short]) extends IntStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): IntStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingShortStepper(s)
    }
  }

  private[collection] class UnboxingFloatStepper(st: AnyStepper[Float]) extends DoubleStepper {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Double = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): DoubleStepper = {
      val s = st.trySplit()
      if (s == null) null else new UnboxingFloatStepper(s)
    }
  }
}

trait AnyStepper[A] extends Stepper[A] {
  private[collection] def trySplit(): AnyStepper[A]

  def spliterator: Spliterator[A] = new AnyStepper.AnyStepperSpliterator(this)

  def javaIterator: JIterator[A] = new JIterator[A] {
    def hasNext: Boolean = hasStep
    def next(): A = nextStep()
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
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): AnyStepper[Double] = {
      val s = st.trySplit()
      if (s == null) null else new BoxedDoubleStepper(s)
    }
  }

  private[collection] class BoxedIntStepper(st: IntStepper) extends AnyStepper[Int] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Int = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): AnyStepper[Int] = {
      val s = st.trySplit()
      if (s == null) null else new BoxedIntStepper(s)
    }
  }

  private[collection] class BoxedLongStepper(st: LongStepper) extends AnyStepper[Long] {
    def hasStep: Boolean = st.hasStep
    def nextStep(): Long = st.nextStep()
    private[collection] def estimateSize: Long = st.estimateSize
    private[collection] def characteristics: Int = st.characteristics
    private[collection] def trySplit(): AnyStepper[Long] = {
      val s = st.trySplit()
      if (s == null) null else new BoxedLongStepper(s)
    }
  }
}


trait IntStepper extends Stepper[Int] {
  private[collection] def trySplit(): IntStepper

  def spliterator: Spliterator.OfInt = new IntStepper.IntStepperSpliterator(this)

  def javaIterator: PrimitiveIterator.OfInt = new PrimitiveIterator.OfInt {
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

trait DoubleStepper extends Stepper[Double] {
  private[collection] def trySplit(): DoubleStepper

  def spliterator: Spliterator.OfDouble = new DoubleStepper.DoubleStepperSpliterator(this)

  def javaIterator: PrimitiveIterator.OfDouble = new PrimitiveIterator.OfDouble {
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

trait LongStepper extends Stepper[Long] {
  private[collection] def trySplit(): LongStepper

  def spliterator: Spliterator.OfLong = new LongStepper.LongStepperSpliterator(this)

  def javaIterator: PrimitiveIterator.OfLong = new PrimitiveIterator.OfLong {
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
