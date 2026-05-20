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

package scala.collection.immutable

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

/**
 * Base class for [[LazyList]] to split out code that uses concurrency utilities that are not available
 * on Scala.js. This way, Scala.js does not need to override all of LazyList.
 *
 * This class cannot be a trait because `AtomicReferenceFieldUpdater.newUpdater` checks if the caller
 * class has access to the corresponding field. So it needs to be called in the class where the field is
 * declared (fields are always private in Scala).
 */
abstract class LazyListBase[+A] private[immutable] (initialTail: AnyRef) extends AbstractSeq[A] with Serializable {
  /** See [[LazyList._head]] for the possible states of this field. */
  @volatile private var _tail: AnyRef /* () => LazyList[A] | Thread | InRace | LazyList[A] */ = initialTail

  private[immutable] def rawTail: AnyRef = _tail

  private[immutable] def setRawTail(value: AnyRef): Unit = _tail = value

  @noinline private[immutable] def makeTailUpdater: LazyListBase.TailUpdater =
    new LazyListBase.TailUpdater(AtomicReferenceFieldUpdater.newUpdater(classOf[LazyListBase[_]], classOf[AnyRef], "_tail"))
}

private[immutable] object LazyListBase {
  final class TailUpdater(u: AtomicReferenceFieldUpdater[LazyListBase[_], AnyRef]) {
    def compareAndSet(ll: LazyListBase[_], expected: AnyRef, value: AnyRef): Boolean = u.compareAndSet(ll, expected, value)
    def getAndSet(ll: LazyListBase[_], value: AnyRef): AnyRef = u.getAndSet(ll, value)
  }

  // this utility is constant `true` on Scala.js -> enables DCE in LazyList
  def isCurrentThread(t: Thread) = t eq Thread.currentThread
  // also for Scala.js
  def InRace(t: Thread) = new InRace(t)

  final class InRace private[LazyListBase] (val owner: Thread) {
    private val done: CountDownLatch = new CountDownLatch(1)

    def await(): Unit = {
      var interrupted = false
      while (done.getCount > 0) {
        try done.await() catch {
          case _: InterruptedException => interrupted = true
        }
      }
      if (interrupted) Thread.currentThread().interrupt()
    }

    def countDown(): Unit = done.countDown()
  }
}
