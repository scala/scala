package scala.collection.immutable


import java.lang.Integer.bitCount
import java.lang.Math.ceil
import java.lang.System.arraycopy

private[immutable] final object Node {

  final val HashCodeLength = 32

  final val BitPartitionSize = 5

  final val BitPartitionMask = (1 << BitPartitionSize) - 1

  final val MaxDepth = ceil(HashCodeLength.toDouble / BitPartitionSize).toInt

  final val SizeEmpty = 0

  final val SizeOne = 1

  final val SizeMoreThanOne = 2

  final def maskFrom(hash: Int, shift: Int): Int = (hash >>> shift) & BitPartitionMask

  final def bitposFrom(mask: Int): Int = 1 << mask

  final def indexFrom(bitmap: Int, bitpos: Int): Int = bitCount(bitmap & (bitpos - 1))

  final def indexFrom(bitmap: Int, mask: Int, bitpos: Int): Int = if (bitmap == -1) mask else indexFrom(bitmap, bitpos)

}

private[immutable] abstract class Node[T <: Node[T]] {

  def hasNodes: Boolean

  def nodeArity: Int

  def getNode(index: Int): T

  def hasPayload: Boolean

  def payloadArity: Int

  def getPayload(index: Int): Any

  def getHash(index: Int): Int

  def sizePredicate: Int

  protected final def removeElement(as: Array[Int], ix: Int): Array[Int] = {
    if (ix < 0) throw new ArrayIndexOutOfBoundsException
    if (ix > as.length - 1) throw new ArrayIndexOutOfBoundsException
    val result = new Array[Int](as.length - 1)
    arraycopy(as, 0, result, 0, ix)
    arraycopy(as, ix + 1, result, ix, as.length - ix - 1)
    result
  }

  protected final def insertElement(as: Array[Int], ix: Int, elem: Int): Array[Int] = {
    if (ix < 0) throw new ArrayIndexOutOfBoundsException
    if (ix > as.length) throw new ArrayIndexOutOfBoundsException
    val result = new Array[Int](as.length + 1)
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
  *
  * @author   Michael J. Steindorfer
  */
private[immutable] abstract class ChampBaseIterator[T <: Node[T]] {

  import Node.MaxDepth

  protected var currentValueCursor: Int = 0
  protected var currentValueLength: Int = 0
  protected var currentValueNode: T = _

  private[this] var currentStackLevel: Int = -1
  private[this] val nodeCursorsAndLengths: Array[Int] = new Array[Int](MaxDepth * 2)
  private[this] val nodes: Array[T] = new Array[Node[T]](MaxDepth).asInstanceOf[Array[T]]

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
  *
  * @author   Michael J. Steindorfer
  */
private[immutable] abstract class ChampBaseReverseIterator[T <: Node[T]] {

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
