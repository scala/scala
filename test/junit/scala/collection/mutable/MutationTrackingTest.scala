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
package mutable

import java.util.ConcurrentModificationException

import org.junit.Test

import scala.annotation.nowarn
import scala.tools.testkit.AssertUtil.assertThrows

abstract class MutationTrackingTest[+C <: Iterable[_]](factory: Factory[Int, C]) {
  private def runOp(op: C => Any, viewOrIterator: C => IterableOnceOps[_, AnyConstr, _]): Unit = {
    val coll = (factory.newBuilder += 1 += 2 += 3 += 4).result()
    val it = viewOrIterator(coll)
    op(coll)
    it.foreach(_ => ())
  }

  private def runOpMaybeThrowing(op: C => Any,
                                 throws: Boolean,
                                 viewOrIterator: C => IterableOnceOps[_, AnyConstr, _]): Unit = {
    if (throws) assertThrows[ConcurrentModificationException](runOp(op, viewOrIterator), _ contains "iteration")
    else runOp(op, viewOrIterator)
  }

  private def runOpForViewAndIterator(op: C => Any, throws: Boolean): Unit = {
    runOp(op, _.view) // never throws
    runOpMaybeThrowing(op, throws, _.iterator)
    runOpMaybeThrowing(op, throws, _.view.iterator)
  }

  /** Checks that no exception is thrown by an operation. */
  def checkFine(op: C => Any): Unit = runOpForViewAndIterator(op, throws = false)

  /** Checks that an exception is thrown by an operation. */
  def checkThrows(op: C => Any): Unit = runOpForViewAndIterator(op, throws = true)

  @Test
  def nop(): Unit = checkFine { _ => () }

  @Test
  def knownSize(): Unit = checkFine { _.knownSize }
}

// mixins
object MutationTrackingTest {
  type I = Iterable[_]

  trait ClearableTest { self: MutationTrackingTest[Clearable with I] =>
    @Test
    def clear(): Unit = checkThrows { _.clear() }
  }

  trait GrowableTest extends ClearableTest { self: MutationTrackingTest[Growable[Int] with I] =>
    @Test
    def addOne(): Unit = checkThrows { _ += 1 }

    @Test
    def addAll(): Unit = {
      checkThrows { _ ++= Seq(2, 3) }
      checkFine   { _ ++= Nil }
    }
  }

  trait ShrinkableTest { self: MutationTrackingTest[Shrinkable[Int] with I] =>
    @Test
    def subtractOne(): Unit = checkThrows { _ -= 1 }

    @Test
    def subtractAll(): Unit = {
      checkThrows { _ --= Seq(1, 2) }
      checkFine   { _ --= Nil }
    }
  }

  trait SeqTest { self: MutationTrackingTest[Seq[Int]] =>
    @Test
    def update(): Unit = checkThrows { _(1) = 5 }

    @Test
    @nowarn("cat=deprecation")
    def transform(): Unit = checkThrows { _.transform(_ + 1) }
  }

  trait BufferTest extends GrowableTest with ShrinkableTest with SeqTest { self: MutationTrackingTest[Buffer[Int]] =>
    @Test
    def insert(): Unit = checkThrows { _.insert(0, 5) }

    @Test
    def insertAll(): Unit = {
      checkThrows { _.insertAll(1, Seq(1, 2, 3)) }
      checkFine   { _.insertAll(1, Nil) }
    }

    @Test
    def remove1(): Unit = checkThrows { _ remove 1 }

    @Test
    def remove2(): Unit = {
      checkThrows { _.remove(1, 2) }
      checkFine   { _.remove(1, 0) }
    }

    @Test
    def prepend(): Unit = checkThrows { _ prepend 5 }

    @Test
    def prependAll(): Unit = {
      checkThrows { _ prependAll Seq(1, 2, 3) }
      checkFine   { _ prependAll Nil }
    }

    @Test
    def patchInPlace(): Unit = {
      checkThrows { _.patchInPlace(1, Seq(2, 3), 3) }
      checkThrows { _.patchInPlace(1, Seq(2, 3), 0) }
      checkThrows { _.patchInPlace(4, Seq(2, 3), 3) }
      checkThrows { _.patchInPlace(1, Nil, 3) }
      checkFine   { _.patchInPlace(1, Nil, 0) }
      checkFine   { _.patchInPlace(4, Nil, 3) }
    }

    @Test
    def dropInPlace(): Unit = {
      checkThrows { _ dropInPlace 2 }
      checkFine   { _ dropInPlace 0 }
    }

    @Test
    def dropRightInPlace(): Unit = {
      checkThrows { _ dropRightInPlace 2 }
      checkFine   { _ dropRightInPlace 0 }
    }

    @deprecated("Tests deprecated API", since="2.13.4")
    @Test
    def trimStart(): Unit = {
      checkThrows { _ trimStart 2 }
      checkFine   { _ trimStart 0 }
    }

    @deprecated("Tests deprecated API", since="2.13.4")
    @Test
    def trimEnd(): Unit = {
      checkThrows { _ trimEnd 2 }
      checkFine   { _ trimEnd 0 }
    }

    @Test
    def takeInPlace(): Unit = {
      checkThrows { _ takeInPlace 2 }
      checkFine   { _ takeInPlace 4 }
    }

    @Test
    def takeRightInPlace(): Unit = {
      checkThrows { _ takeRightInPlace 2 }
      checkFine   { _ takeRightInPlace 4 }
    }

    @Test
    def sliceInPlace(): Unit = {
      checkThrows { _.sliceInPlace(1, 3) }
      checkFine   { _.sliceInPlace(0, 4) }
    }

    @Test
    def dropWhileInPlace(): Unit = {
      checkThrows { _.dropWhileInPlace(_ < 3) }
      checkFine   { _.dropWhileInPlace(_ => false) }
    }

    @Test
    def takeWhileInPlace(): Unit = {
      checkThrows { _.takeWhileInPlace(_ < 3) }
      checkFine   { _.takeWhileInPlace(_ => true) }
    }

    @Test
    def padToInPlace(): Unit = {
      checkThrows { _.padToInPlace(5, 0) }
      checkFine   { _.padToInPlace(2, 0) }
    }
  }
}

// concrete tests
package MutationTrackingTestImpl {
  import scala.collection.mutable.MutationTrackingTest._

  class ListBufferTest extends MutationTrackingTest(ListBuffer) with BufferTest {
    @Test
    def mapInPlace(): Unit = checkThrows { _.mapInPlace(_ + 1) }

    @Test
    def flatMapInPlace(): Unit = checkThrows { _.flatMapInPlace(i => (i + 1) :: Nil) }

    @Test
    def filterInPlace(): Unit = checkThrows { _.filterInPlace(_ => true) }
  }
}
