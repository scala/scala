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

import annotation.tailrec
import scala.collection.Stepper.EfficientSplit
import scala.collection._
import scala.collection.immutable.Node

/** A stepper that is a slightly elaborated version of the ChampBaseIterator;
  * the main difference is that it knows when it should stop instead of running
  * to the end of all trees.
  */
private[collection] abstract class ChampStepperBase[
  A, T <: Node[T], Sub >: Null, Semi <: Sub with ChampStepperBase[A, T, _, _]
](protected var maxSize: Int)
extends EfficientSplit {
  import Node.MaxDepth

  // Most of this code is identical to ChampBaseIterator.  If you change that, look here too!

  protected var currentValueCursor: Int = 0
  protected var currentValueLength: Int = 0
  protected var currentValueNode: T = _

  private var currentStackLevel: Int = -1
  private var nodeCursorsAndLengths: Array[Int] = _
  private var nodes: Array[T] = _

  private def initNodes(): Unit = {
    if (nodeCursorsAndLengths eq null) {
      nodeCursorsAndLengths = new Array[Int](MaxDepth * 2)
      nodes = new Array[Node[T]](MaxDepth).asInstanceOf[Array[T]]
    }
  }
  protected def effectiveRootLevel: Int =
    if (currentStackLevel < 0) -1
    else {
      var erl = 0
      while (erl < nodeCursorsAndLengths.length) {
        if (nodeCursorsAndLengths(erl) < nodeCursorsAndLengths(erl+1)) return (erl >>> 1)
        erl += 2
      }
      -1
    }
  protected def skipHalf(rootLevel: Int): Int = {
    val cursorAtRoot = nodeCursorsAndLengths(2*rootLevel)
    val lengthAtRoot = nodeCursorsAndLengths(2*rootLevel+1)
    val last = (cursorAtRoot + lengthAtRoot) >>> 1
    currentStackLevel = rootLevel
    nodeCursorsAndLengths(2*rootLevel) = last
    searchNextValueNode()
    last
  }
  protected def stealNodesFrom(that: ChampStepperBase[A, T, _, _], rootLevel: Int): Unit = {
    initNodes()
    maxSize            = that.maxSize
    currentValueCursor = that.currentValueCursor
    currentValueNode   = that.currentValueNode
    currentStackLevel  = that.currentStackLevel - rootLevel
    var i = 0
    while (i <= currentStackLevel) {
      val j = i + rootLevel
      nodeCursorsAndLengths(2*i)   = that.nodeCursorsAndLengths(2*j)
      nodeCursorsAndLengths(2*i+1) = that.nodeCursorsAndLengths(2*j+1)
      nodes(i)                     = that.nodes(j)
      i += 1
    }
    val last = that.skipHalf(rootLevel)
    nodeCursorsAndLengths(2*rootLevel + 1) = last-1
  }
  protected def stealCurrentNodesFrom(that: ChampStepperBase[A, T, _, _]): Unit = {
    maxSize            = that.maxSize
    currentValueCursor = that.currentValueCursor
    currentValueNode   = that.currentValueNode
    currentStackLevel  = -1
    currentValueLength = (currentValueCursor + that.currentValueLength) >>> 1
    that.currentValueCursor = currentValueLength
  }

  def initRoot(rootNode: T): Unit = {
    if (rootNode.hasNodes) pushNode(rootNode)
    if (rootNode.hasPayload) setupPayloadNode(rootNode)
  }

  private final def setupPayloadNode(node: T): Unit = {
    currentValueNode = node
    currentValueCursor = 0
    currentValueLength = node.payloadArity
  }

  private final def pushNode(node: T): Unit = {
    initNodes()
    currentStackLevel = currentStackLevel + 1

    val cursorIndex = currentStackLevel * 2
    val lengthIndex = currentStackLevel * 2 + 1

    nodes(currentStackLevel) = node
    nodeCursorsAndLengths(cursorIndex) = 0
    nodeCursorsAndLengths(lengthIndex) = node.nodeArity
  }

  private final def popNode(): Unit = {
    currentStackLevel = currentStackLevel - 1
  }

  /**
    * Searches for next node that contains payload values,
    * and pushes encountered sub-nodes on a stack for depth-first traversal.
    */
  private final def searchNextValueNode(): Boolean = {
    while (currentStackLevel >= 0) {
      val cursorIndex = currentStackLevel * 2
      val lengthIndex = currentStackLevel * 2 + 1

      val nodeCursor = nodeCursorsAndLengths(cursorIndex)
      val nodeLength = nodeCursorsAndLengths(lengthIndex)

      if (nodeCursor < nodeLength) {
        nodeCursorsAndLengths(cursorIndex) += 1

        val nextNode = nodes(currentStackLevel).getNode(nodeCursor)

        if (nextNode.hasNodes)   { pushNode(nextNode) }
        if (nextNode.hasPayload) { setupPayloadNode(nextNode) ; return true }
      } else {
        popNode()
      }
    }
    false
  }

  def characteristics: Int = 0

  def estimateSize: Long = if (hasStep) maxSize else 0L

  def semiclone(): Semi

  final def hasStep: Boolean = maxSize > 0 && {
    val ans = (currentValueCursor < currentValueLength) || searchNextValueNode()
    if (!ans) maxSize = 0
    ans
  }

  final def trySplit(): Sub =
    if (!hasStep) null
    else {
      val root = effectiveRootLevel
      if (root < 0) {
        if (currentValueCursor > currentValueLength-2) null
        else {
          val semi = semiclone()
          semi.stealCurrentNodesFrom(this)
          semi
        }
      }
      else {
        val semi = semiclone()
        semi.stealNodesFrom(this, root)
        semi
      }
    }
}


private[collection] final class AnyChampStepper[A, T >: Null <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => A)
extends ChampStepperBase[A, T, AnyStepper[A], AnyChampStepper[A, T]](_maxSize)
with AnyStepper[A] {
  def nextStep(): A =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(): AnyChampStepper[A, T] = new AnyChampStepper[A, T](0, extract)
}
private[collection] object AnyChampStepper {
  def from[A, T >: Null <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => A): AnyChampStepper[A, T] = {
    val ans = new AnyChampStepper[A, T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}

private[collection] final class DoubleChampStepper[T >: Null <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => Double)
extends ChampStepperBase[Double, T, DoubleStepper, DoubleChampStepper[T]](_maxSize)
with DoubleStepper {
  def nextStep(): Double =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(): DoubleChampStepper[T] = new DoubleChampStepper[T](0, extract)
}
private[collection] object DoubleChampStepper {
  def from[T >: Null <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => Double): DoubleChampStepper[T] = {
    val ans = new DoubleChampStepper[T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}

private[collection] final class IntChampStepper[T >: Null <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => Int)
extends ChampStepperBase[Int, T, IntStepper, IntChampStepper[T]](_maxSize)
with IntStepper {
  def nextStep(): Int =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(): IntChampStepper[T] = new IntChampStepper[T](0, extract)
}
private[collection] object IntChampStepper {
  def from[T >: Null <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => Int): IntChampStepper[T] = {
    val ans = new IntChampStepper[T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}

private[collection] final class LongChampStepper[T >: Null <: Node[T]](_maxSize: Int, protected val extract: (T, Int) => Long)
extends ChampStepperBase[Long, T, LongStepper, LongChampStepper[T]](_maxSize)
with LongStepper {
  def nextStep(): Long =
    if (hasStep) {
      val ans = extract(currentValueNode, currentValueCursor)
      currentValueCursor += 1
      maxSize -= 1
      ans
    }
    else Stepper.throwNSEE()

  def semiclone(): LongChampStepper[T] = new LongChampStepper[T](0, extract)
}
private[collection] object LongChampStepper {
  def from[T >: Null <: Node[T]](maxSize: Int, root: T, extract: (T, Int) => Long): LongChampStepper[T] = {
    val ans = new LongChampStepper[T](maxSize, extract)
    ans.initRoot(root)
    ans
  }
}
