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

import scala.collection.AbstractIterator
import java.lang.Integer.bitCount
import java.lang.Math.ceil
import java.lang.System.arraycopy

private[collection] object Node {
  final val HashCodeLength = 32

  final val BitPartitionSize = 5

  final val BitPartitionMask = (1 << BitPartitionSize) - 1

  final val MaxDepth = ceil(HashCodeLength.toDouble / BitPartitionSize).toInt

  final val BranchingFactor = 1 << BitPartitionSize

  final def maskFrom(hash: Int, shift: Int): Int = (hash >>> shift) & BitPartitionMask

  final def bitposFrom(mask: Int): Int = 1 << mask

  final def indexFrom(bitmap: Int, bitpos: Int): Int = bitCount(bitmap & (bitpos - 1))

  final def indexFrom(bitmap: Int, mask: Int, bitpos: Int): Int = if (bitmap == -1) mask else indexFrom(bitmap, bitpos)

}

private[collection] abstract class Node[T <: Node[T]] {

  def hasNodes: Boolean

  def nodeArity: Int

  def getNode(index: Int): T

  def hasPayload: Boolean

  def payloadArity: Int

  def getPayload(index: Int): Any

  def getHash(index: Int): Int

  def cachedJavaKeySetHashCode: Int

  private final def arrayIndexOutOfBounds(as: Array[_], ix:Int): ArrayIndexOutOfBoundsException =
    new ArrayIndexOutOfBoundsException(s"$ix is out of bounds (min 0, max ${as.length-1}")

  protected final def removeElement(as: Array[Int], ix: Int): Array[Int] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length - 1) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Int](as.length - 1)
    arraycopy(as, 0, result, 0, ix)
    arraycopy(as, ix + 1, result, ix, as.length - ix - 1)
    result
  }

  protected final def removeAnyElement(as: Array[Any], ix: Int): Array[Any] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length - 1) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Any](as.length - 1)
    arraycopy(as, 0, result, 0, ix)
    arraycopy(as, ix + 1, result, ix, as.length - ix - 1)
    result
  }

  protected final def insertElement(as: Array[Int], ix: Int, elem: Int): Array[Int] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Int](as.length + 1)
    arraycopy(as, 0, result, 0, ix)
    result(ix) = elem
    arraycopy(as, ix, result, ix + 1, as.length - ix)
    result
  }
  protected final def insertAnyElement(as: Array[Any], ix: Int, elem: Int): Array[Any] = {
    if (ix < 0) throw arrayIndexOutOfBounds(as, ix)
    if (ix > as.length) throw arrayIndexOutOfBounds(as, ix)
    val result = new Array[Any](as.length + 1)
    arraycopy(as, 0, result, 0, ix)
    result(ix) = elem
    arraycopy(as, ix, result, ix + 1, as.length - ix)
    result
  }
}

/**
  * Base class for fixed-stack iterators that traverse a hash-trie. The iterator performs a
  * depth-first pre-order traversal, which yields first all payload elements of the current
  * node before traversing sub-nodes (left to right).
  *
  * @tparam T the trie node type we are iterating over
  */
private[immutable] abstract class ChampBaseIterator[A, T <: Node[T]] extends AbstractIterator[A] {

  import Node.MaxDepth

  // Note--this code is duplicated to a large extent both in
  // ChampBaseReverseIterator and in convert.impl.ChampStepperBase.
  // If you change this code, check those also in case they also
  // need to be modified.
  
  protected var currentValueCursor: Int = 0
  protected var currentValueLength: Int = 0
  protected var currentValueNode: T = _

  private[this] var currentStackLevel: Int = -1
  private[this] var nodeCursorsAndLengths: Array[Int] = _
  private[this] var nodes: Array[T] = _
  private def initNodes(): Unit = {
    if (nodeCursorsAndLengths eq null) {
      nodeCursorsAndLengths = new Array[Int](MaxDepth * 2)
      nodes = new Array[Node[T]](MaxDepth).asInstanceOf[Array[T]]
    }
  }

  def this(rootNode: T) = {
    this()
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

    return false
  }

  final def hasNext = (currentValueCursor < currentValueLength) || searchNextValueNode()

}

/**
  * Base class for fixed-stack iterators that traverse a hash-trie in reverse order. The base
  * iterator performs a depth-first post-order traversal, traversing sub-nodes (right to left).
  *
  * @tparam T the trie node type we are iterating over
  */
private[immutable] abstract class ChampBaseReverseIterator[A, T <: Node[T]] extends AbstractIterator[A] {

  import Node.MaxDepth

  protected var currentValueCursor: Int = -1
  protected var currentValueNode: T = _

  private[this] var currentStackLevel: Int = -1
  private[this] val nodeIndex: Array[Int] = new Array[Int](MaxDepth + 1)
  private[this] val nodeStack: Array[T] = new Array[Node[T]](MaxDepth + 1).asInstanceOf[Array[T]]

  def this(rootNode: T) = {
    this()
    pushNode(rootNode)
    searchNextValueNode()
  }

  private final def setupPayloadNode(node: T): Unit = {
    currentValueNode = node
    currentValueCursor = node.payloadArity - 1
  }

  private final def pushNode(node: T): Unit = {
    currentStackLevel = currentStackLevel + 1

    nodeStack(currentStackLevel) = node
    nodeIndex(currentStackLevel) = node.nodeArity - 1
  }

  private final def popNode(): Unit = {
    currentStackLevel = currentStackLevel - 1
  }

  /**
    * Searches for rightmost node that contains payload values,
    * and pushes encountered sub-nodes on a stack for depth-first traversal.
    */
  private final def searchNextValueNode(): Boolean = {
    while (currentStackLevel >= 0) {
      val nodeCursor = nodeIndex(currentStackLevel) ; nodeIndex(currentStackLevel) = nodeCursor - 1

      if (nodeCursor >= 0) {
        val nextNode = nodeStack(currentStackLevel).getNode(nodeCursor)
        pushNode(nextNode)
      } else {
        val currNode = nodeStack(currentStackLevel)
        popNode()

        if (currNode.hasPayload) { setupPayloadNode(currNode) ; return true }
      }
    }

    return false
  }

  final def hasNext = (currentValueCursor >= 0) || searchNextValueNode()

}
