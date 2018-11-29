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
package impl

private[collection] class AnyIteratorStepper[A](_underlying: Iterator[A])
  extends IteratorStepperBase[A, AnyStepper[A], AnyIteratorStepper[A]](_underlying)
    with AnyStepper[A] {
  protected def semiclone(): AnyIteratorStepper[A] = new AnyIteratorStepper(null)

  def next(): A = if (proxied ne null) proxied.nextStep() else underlying.next()

  override def substep(): AnyStepper[A] = if (proxied ne null) proxied.substep() else {
    val acc = new Accumulator[A]
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.hasNext) { acc += underlying.next(); i += 1 }
    if (i < n || !underlying.hasNext) {
      proxied = acc.stepper
      proxied.substep()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

private[collection] class DoubleIteratorStepper(_underlying: Iterator[Double])
  extends IteratorStepperBase[Double, DoubleStepper, DoubleIteratorStepper](_underlying)
    with DoubleStepper {
  protected def semiclone(): DoubleIteratorStepper = new DoubleIteratorStepper(null)

  def nextDouble(): Double = if (proxied ne null) proxied.nextStep() else underlying.next()

  override def substep(): DoubleStepper = if (proxied ne null) proxied.substep() else {
    val acc = new DoubleAccumulator
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.hasNext) { acc += underlying.next(); i += 1 }
    if (i < n || !underlying.hasNext) {
      proxied = acc.stepper
      proxied.substep()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

private[collection] class IntIteratorStepper(_underlying: Iterator[Int])
  extends IteratorStepperBase[Int, IntStepper, IntIteratorStepper](_underlying)
    with IntStepper {
  protected def semiclone(): IntIteratorStepper = new IntIteratorStepper(null)

  def nextInt(): Int = if (proxied ne null) proxied.nextStep() else underlying.next()

  override def substep(): IntStepper = if (proxied ne null) proxied.substep() else {
    val acc = new IntAccumulator
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.hasNext) { acc += underlying.next(); i += 1 }
    if (i < n || !underlying.hasNext) {
      proxied = acc.stepper
      proxied.substep()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

private[collection] class LongIteratorStepper(_underlying: Iterator[Long])
  extends IteratorStepperBase[Long, LongStepper, LongIteratorStepper](_underlying)
    with LongStepper {
  protected def semiclone(): LongIteratorStepper = new LongIteratorStepper(null)

  def nextLong(): Long = if (proxied ne null) proxied.nextStep() else underlying.next()

  override def substep(): LongStepper = if (proxied ne null) proxied.substep() else {
    val acc = new LongAccumulator
    var i = 0
    val n = nextChunkSize & 0xFFFFFFFC
    while (i < n && underlying.hasNext) { acc += underlying.next(); i += 1 }
    if (i < n || !underlying.hasNext) {
      proxied = acc.stepper
      proxied.substep()
    }
    else {
      val ans = semiclone()
      ans.proxied = acc.stepper
      nextChunkSize = if ((nextChunkSize&3) == 3) { if (n < 0x40000000) n*2 else n } else nextChunkSize + 1
      ans
    }
  }
}

/** Common functionality for Steppers that step through an Iterator, caching the results as needed when a split is requested. */
private[convert] abstract class IteratorStepperBase[A, SP >: Null <: Stepper[A], Semi <: SP](final protected var underlying: Iterator[A]) {
  final protected var nextChunkSize = 16
  final protected var proxied: SP = null
  protected def semiclone(): Semi        // Must initialize with null iterator!
  def characteristics: Int = if (proxied ne null) Stepper.Ordered | Stepper.Sized | Stepper.SubSized else Stepper.Ordered
  def estimateSize(): Long = if (proxied ne null) proxied.knownSize else Long.MaxValue
  def hasNext: Boolean = if (proxied ne null) proxied.hasStep else underlying.hasNext
}
