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

package scala.collection.convert
package impl

import java.util.Spliterator

import annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection._


private[collection] object BinaryTreeStepper {
  val emptyStack = new Array[AnyRef](0)
}


/** A generic stepper that can traverse ordered binary trees.
  * The tree is assumed to have all the stuff on the left first, then the root, then everything on the right.
  *
  * Splits occur at the root of whatever has not yet been traversed (the substepper steps up to but
  * does not include the root).
  *
  * The stepper maintains an internal stack, not relying on the tree traversal to be reversible.  Trees with
  * nodes that maintain a parent pointer may be traversed slightly faster without a stack, but splitting is
  * more awkward.
  *
  * Algorithmically, this class implements a simple state machine that unrolls the left-leaning links in
  * a binary tree onto a stack.  At all times, the machine should be in one of these states:
  * 1. Empty: `myCurrent` is `null` and `index` is `-1`.  `stack` should also be `Array.empty` then.
  * 2. Ready: `myCurrent` is not `null` and contains the next `A` to be extracted
  * 3. Pending: `myCurrent` is `null` and `stack(index)` contains the next node to visit
  *
  * Subclasses should allow this class to do all the work of maintaining state; `next` should simply
  * reduce `maxLength` by one, and consume `myCurrent` and set it to `null` if `hasNext` is true.
  */
private[collection] abstract class BinaryTreeStepperBase[A, T >: Null <: AnyRef, Sub >: Null, Semi <: Sub with BinaryTreeStepperBase[A, T, _, _]](
  protected var maxLength: Int, protected var myCurrent: T, protected var stack: Array[AnyRef], protected var index: Int,
  protected val left: T => T, protected val right: T => T
)
extends EfficientSplit {
  /** Unrolls a subtree onto the stack starting from a particular node, returning
    * the last node found.  This final node is _not_ placed on the stack, and
    * may have things to its right.
    */
  @tailrec protected final def unroll(from: T): T = {
    val l = left(from)
    if (l eq null) from
    else {
      if (index+1 >= stack.length) stack = java.util.Arrays.copyOf(stack, 4 + stack.length*2)
      index += 1
      stack(index) = from
      unroll(l)
    }
  }

  /** Takes a subtree whose left side, if any, has already been visited, and unrolls
    * the right side of the tree onto the stack, thereby detaching that node of
    * the subtree from the stack entirely (so it is ready to use).  It returns
    * the node that is being detached. Note that the node must _not_ already be
    * on the stack.
    */
  protected final def detach(node: T): node.type = {
    val r = right(node)
    if (r ne null) {
      val last = unroll(r)
      if (index+1 >= stack.length) stack = java.util.Arrays.copyOf(stack, 4 + stack.length*2)
      index += 1
      stack(index) = last
    }
    node
  }

  /** Given an empty state and the root of a new tree, initialize the tree properly
    * to be in an (appropriate) ready state.  Will do all sorts of wrong stuff if the
    * tree is not already empty.
    *
    * Right now overwrites everything so could allow reuse, but isn't used for it.
    */
  private[impl] final def initialize(root: T, size: Int): Unit =
    if (root eq null) {
      maxLength = 0
      myCurrent = null
      stack = BinaryTreeStepper.emptyStack
      index = -1
    }
    else {
      maxLength = size
      index = -1
      myCurrent = detach(unroll(root))
    }

  protected def semiclone(maxL: Int, myC: T, stk: Array[AnyRef], ix: Int): Semi

  def characteristics: Int = Spliterator.ORDERED

  def estimateSize: Long = if (hasStep) maxLength else 0

  def hasStep: Boolean = (myCurrent ne null) || (maxLength > 0 && {
    if (index < 0) { maxLength = 0; stack = BinaryTreeStepper.emptyStack; false }
    else {
      val ans = stack(index).asInstanceOf[T]
      index -= 1
      myCurrent = detach(ans)
      true
    }
  })

  /** Splits the tree at the root by giving everything unrolled on the stack to a new stepper,
    * detaching the root, and leaving the right-hand side of the root unrolled.
    *
    * If the tree is empty or only has one element left, it returns `null` instead of splitting.
    */
  def trySplit(): Sub =
    if (!hasStep || index < 0) null
    else {
      val root = stack(0).asInstanceOf[T]
      val leftStack = 
        if (index > 0) java.util.Arrays.copyOfRange(stack, 1, index+1)
        else BinaryTreeStepper.emptyStack
      val leftIndex = index - 1
      val leftCurrent = myCurrent
      var leftMax = maxLength
      index = -1
      detach(root)
      myCurrent = root
      leftMax -= 2+index
      maxLength -= 2+leftIndex
      semiclone(leftMax, leftCurrent, leftStack, leftIndex)
    }
}


private[collection] final class AnyBinaryTreeStepper[A, T >: Null <: AnyRef](
  _maxLength: Int, _myCurrent: T, _stack: Array[AnyRef], _index: Int, _left: T => T, _right: T => T, protected val extract: T => A
)
extends BinaryTreeStepperBase[A, T, AnyStepper[A], AnyBinaryTreeStepper[A, T]](_maxLength, _myCurrent, _stack, _index, _left, _right)
with AnyStepper[A] {
  def nextStep(): A =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = null
      maxLength -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(maxL: Int, myC: T, stk: Array[AnyRef], ix: Int): AnyBinaryTreeStepper[A, T] =
    new AnyBinaryTreeStepper[A, T](maxL, myC, stk, ix, left, right, extract)
}
private[collection] object AnyBinaryTreeStepper {
  def from[A, T >: Null <: AnyRef](maxLength: Int, root: T, left: T => T, right: T => T, extract: T => A): AnyBinaryTreeStepper[A, T] = {
    val ans = new AnyBinaryTreeStepper(0, null, BinaryTreeStepper.emptyStack, -1, left, right, extract)
    ans.initialize(root, maxLength)
    ans
  }
}


private[collection] final class DoubleBinaryTreeStepper[T >: Null <: AnyRef](
  _maxLength: Int, _myCurrent: T, _stack: Array[AnyRef], _index: Int, _left: T => T, _right: T => T, protected val extract: T => Double
)
extends BinaryTreeStepperBase[Double, T, DoubleStepper, DoubleBinaryTreeStepper[T]](_maxLength, _myCurrent, _stack, _index, _left, _right)
with DoubleStepper {
  def nextStep(): Double =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = null
      maxLength -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(maxL: Int, myC: T, stk: Array[AnyRef], ix: Int): DoubleBinaryTreeStepper[T] =
    new DoubleBinaryTreeStepper[T](maxL, myC, stk, ix, left, right, extract)
}
private [collection] object DoubleBinaryTreeStepper {
  def from[T >: Null <: AnyRef](maxLength: Int, root: T, left: T => T, right: T => T, extract: T => Double): DoubleBinaryTreeStepper[T] = {
    val ans = new DoubleBinaryTreeStepper(0, null, BinaryTreeStepper.emptyStack, -1, left, right, extract)
    ans.initialize(root, maxLength)
    ans
  }
}


private[collection] final class IntBinaryTreeStepper[T >: Null <: AnyRef](
  _maxLength: Int, _myCurrent: T, _stack: Array[AnyRef], _index: Int, _left: T => T, _right: T => T, protected val extract: T => Int
)
extends BinaryTreeStepperBase[Int, T, IntStepper, IntBinaryTreeStepper[T]](_maxLength, _myCurrent, _stack, _index, _left, _right)
with IntStepper {
  def nextStep(): Int =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = null
      maxLength -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(maxL: Int, myC: T, stk: Array[AnyRef], ix: Int): IntBinaryTreeStepper[T] =
    new IntBinaryTreeStepper[T](maxL, myC, stk, ix, left, right, extract)
}
private [collection] object IntBinaryTreeStepper {
  def from[T >: Null <: AnyRef](maxLength: Int, root: T, left: T => T, right: T => T, extract: T => Int): IntBinaryTreeStepper[T] = {
    val ans = new IntBinaryTreeStepper(0, null, BinaryTreeStepper.emptyStack, -1, left, right, extract)
    ans.initialize(root, maxLength)
    ans
  }
}



private[collection] final class LongBinaryTreeStepper[T >: Null <: AnyRef](
  _maxLength: Int, _myCurrent: T, _stack: Array[AnyRef], _index: Int, _left: T => T, _right: T => T, protected val extract: T => Long
)
extends BinaryTreeStepperBase[Long, T, LongStepper, LongBinaryTreeStepper[T]](_maxLength, _myCurrent, _stack, _index, _left, _right)
with LongStepper {
  def nextStep(): Long =
    if (hasStep) {
      val ans = extract(myCurrent)
      myCurrent = null
      maxLength -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(maxL: Int, myC: T, stk: Array[AnyRef], ix: Int): LongBinaryTreeStepper[T] =
    new LongBinaryTreeStepper[T](maxL, myC, stk, ix, left, right, extract)
}
private [collection] object LongBinaryTreeStepper {
  def from[T >: Null <: AnyRef](maxLength: Int, root: T, left: T => T, right: T => T, extract: T => Long): LongBinaryTreeStepper[T] = {
    val ans = new LongBinaryTreeStepper(0, null, BinaryTreeStepper.emptyStack, -1, left, right, extract)
    ans.initialize(root, maxLength)
    ans
  }
}


