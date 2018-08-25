package scala
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import mutable.{Builder, ImmutableBuilder}
import Hashing.improve
import java.lang.Integer.{bitCount, numberOfTrailingZeros}
import java.lang.System.arraycopy

import scala.util.hashing.MurmurHash3

import scala.runtime.Statics.releaseFence

/** This class implements immutable sets using a Compressed Hash-Array Mapped Prefix-tree.
  * See paper https://michael.steindorfer.name/publications/oopsla15.pdf for more details.
  *
  *  @tparam A      the type of the elements contained in this hash set.
  *
  *  @author  Michael J. Steindorfer
  *  @since   2.13
  *  @define Coll `immutable.HashSet`
  *  @define coll immutable champ hash set
  */
final class HashSet[A] private[immutable] (val rootNode: SetNode[A], val cachedJavaHashCode: Int)
  extends AbstractSet[A]
    with SetOps[A, HashSet, HashSet[A]]
    with StrictOptimizedIterableOps[A, HashSet, HashSet[A]] {

  releaseFence()

  override def iterableFactory: IterableFactory[HashSet] = HashSet

  override def knownSize: Int = rootNode.size

  override def size: Int = rootNode.size

  override def isEmpty: Boolean = rootNode.size == 0

  def iterator: Iterator[A] = {
    if (isEmpty) Iterator.empty
    else new SetIterator[A](rootNode)
  }

  protected[immutable] def reverseIterator: Iterator[A] = new SetReverseIterator[A](rootNode)

  def contains(element: A): Boolean = {
    val elementUnimprovedHash = element.##
    val elementHash = improve(elementUnimprovedHash)
    rootNode.contains(element, elementUnimprovedHash, elementHash, 0)
  }

  def incl(element: A): HashSet[A] = {
    val elementUnimprovedHash = element.##
    val elementHash = improve(elementUnimprovedHash)
    val newRootNode = rootNode.updated(element, elementUnimprovedHash, elementHash, 0)

    if (newRootNode ne rootNode) {
      val newCachedJavaHashCode = cachedJavaHashCode + (if (newRootNode.size == rootNode.size) 0 else elementHash)
      HashSet(newRootNode, newCachedJavaHashCode)
    } else this
  }

  def excl(element: A): HashSet[A] = {
    val elementUnimprovedHash = element.##
    val elementHash = improve(elementUnimprovedHash)
    val newRootNode = rootNode.removed(element, elementUnimprovedHash, elementHash, 0)

    if (rootNode ne newRootNode)
      HashSet(newRootNode, cachedJavaHashCode - elementHash)
    else this
  }

  override def tail: HashSet[A] = this - head

  override def init: HashSet[A] = this - last

  override def head: A = iterator.next()

  override def last: A = reverseIterator.next()

  override def foreach[U](f: A => U): Unit = rootNode.foreach(f)

  def subsetOf(that: Set[A]): Boolean = if (that.isEmpty) true else that match {
    case set: HashSet[A] => rootNode.subsetOf(set.rootNode, 0)
    case _ => super.subsetOf(that)
  }

  override def equals(that: Any): Boolean =
    that match {
      case set: HashSet[A] =>
        (this eq set) ||
          (this.size == set.size) &&
            (this.cachedJavaHashCode == set.cachedJavaHashCode) &&
            (this.rootNode == set.rootNode)
      case _ => super.equals(that)
    }

  override protected[this] def className = "HashSet"

  override def hashCode(): Int = {
    val it = new SetHashIterator(rootNode)
    val hash = MurmurHash3.unorderedHash(it, MurmurHash3.setSeed)
    //assert(hash == super.hashCode())
    hash
  }
}

private[immutable] final object SetNode {

  private final val EmptySetNode = new BitmapIndexedSetNode(0, 0, Array.empty, Array.empty, 0)

  def empty[A]: SetNode[A] =
    EmptySetNode.asInstanceOf[SetNode[A]]

  final val TupleLength = 1

}

private[immutable] sealed abstract class SetNode[A] extends Node[SetNode[A]] {

  def contains(element: A, originalHash: Int, hash: Int, shift: Int): Boolean

  def updated(element: A, originalHash: Int, hash: Int, shift: Int): SetNode[A]

  def removed(element: A, originalHash: Int, hash: Int, shift: Int): SetNode[A]

  def hasNodes: Boolean

  def nodeArity: Int

  def getNode(index: Int): SetNode[A]

  def hasPayload: Boolean

  def payloadArity: Int

  def getPayload(index: Int): A

  def sizePredicate: Int

  def size: Int

  def foreach[U](f: A => U): Unit

  def subsetOf(that: SetNode[A], shift: Int): Boolean

}

private final class BitmapIndexedSetNode[A](
  var dataMap: Int,
  var nodeMap: Int,
  var content: Array[Any],
  var originalHashes: Array[Int],
  var size: Int) extends SetNode[A] {

  import Node._
  import SetNode._

  /*
  assert(checkInvariantContentIsWellTyped())
  assert(checkInvariantSubNodesAreCompacted())

  private final def checkInvariantSubNodesAreCompacted(): Boolean =
    new SetIterator[A](this).size - payloadArity >= 2 * nodeArity

  private final def checkInvariantContentIsWellTyped(): Boolean = {
    val predicate1 = TupleLength * payloadArity + nodeArity == content.length

    val predicate2 = Range(0, TupleLength * payloadArity)
      .forall(i => content(i).isInstanceOf[SetNode[_]] == false)

    val predicate3 = Range(TupleLength * payloadArity, content.length)
      .forall(i => content(i).isInstanceOf[SetNode[_]] == true)

    predicate1 && predicate2 && predicate3
  }
  */

  def getPayload(index: Int) =
    content(TupleLength * index).asInstanceOf[A]

  override def getHash(index: Int): Int = originalHashes(index)

  def getNode(index: Int) =
    content(content.length - 1 - index).asInstanceOf[SetNode[A]]

  def contains(element: A, originalHash: Int, elementHash: Int, shift: Int): Boolean = {
    val mask = maskFrom(elementHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      return originalHashes(index) == originalHash && element == this.getPayload(index)
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      return this.getNode(index).contains(element, originalHash, elementHash, shift + BitPartitionSize)
    }

    false
  }

  def updated(element: A, originalHash: Int, elementHash: Int, shift: Int): SetNode[A] = {
    val mask = maskFrom(elementHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val element0 = this.getPayload(index)

      if (element0.asInstanceOf[AnyRef] eq element.asInstanceOf[AnyRef]) {
        return this
      } else {
        if (element0.equals(element)) {
          val element0UnimprovedHash = element0.##
          val element0Hash = improve(element0UnimprovedHash)
          return copyAndSetValue(bitpos, element, originalHash, elementHash)
        } else {
          val element0UnimprovedHash = element0.##
          val element0Hash = improve(element0UnimprovedHash)
          val subNodeNew = mergeTwoKeyValPairs(element0, element0UnimprovedHash, element0Hash, element, originalHash, elementHash, shift + BitPartitionSize)
          return copyAndMigrateFromInlineToNode(bitpos, subNodeNew)
        }
      }
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.updated(element, originalHash, elementHash, shift + BitPartitionSize)
      if (subNode eq subNodeNew) {
        return this
      } else {
        return copyAndSetNode(bitpos, subNode, subNodeNew)
      }
    }

    copyAndInsertValue(bitpos, element, originalHash, elementHash)
  }

  def removed(element: A, originalHash: Int, elementHash: Int, shift: Int): SetNode[A] = {
    val mask = maskFrom(elementHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val element0 = this.getPayload(index)

      if (element0 == element) {
        if (this.payloadArity == 2 && this.nodeArity == 0) {
          /*
           * Create new node with remaining pair. The new node will a) either become the new root
           * returned, or b) unwrapped and inlined during returning.
           */
          val newDataMap = if (shift == 0) (dataMap ^ bitpos) else bitposFrom(maskFrom(elementHash, 0))
          if (index == 0)
            return new BitmapIndexedSetNode[A](newDataMap, 0, Array(getPayload(1)), Array(originalHashes(1)), size - 1)
          else
            return new BitmapIndexedSetNode[A](newDataMap, 0, Array(getPayload(0)), Array(originalHashes(0)), size - 1)
        }
        else return copyAndRemoveValue(bitpos)
      } else return this
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.removed(element, originalHash, elementHash, shift + BitPartitionSize)
      // assert(subNodeNew.sizePredicate != SizeEmpty, "Sub-node must have at least one element.")

      if (subNodeNew eq subNode) return this
      subNodeNew.sizePredicate match {
        case SizeOne =>
          if (this.payloadArity == 0 && this.nodeArity == 1) { // escalate (singleton or empty) result
            return subNodeNew
          }
          else { // inline value (move to front)
            return copyAndMigrateFromNodeToInline(bitpos, elementHash, subNode, subNodeNew)
          }

        case SizeMoreThanOne =>
          // modify current node (set replacement node)
          return copyAndSetNode(bitpos, subNode, subNodeNew)
      }
    }

    this
  }

  def mergeTwoKeyValPairs(key0: A, originalKeyHash0: Int, keyHash0: Int, key1: A, originalKeyHash1: Int, keyHash1: Int, shift: Int): SetNode[A] = {
    // assert(key0 != key1)

    if (shift >= HashCodeLength) {
      new HashCollisionSetNode[A](originalKeyHash0, keyHash0, Array(key0, key1))
    } else {
      val mask0 = maskFrom(keyHash0, shift)
      val mask1 = maskFrom(keyHash1, shift)

      if (mask0 != mask1) {
        // unique prefixes, payload fits on same level
        val dataMap = bitposFrom(mask0) | bitposFrom(mask1)

        if (mask0 < mask1) {
          new BitmapIndexedSetNode[A](dataMap, 0, Array(key0, key1), Array(originalKeyHash0, originalKeyHash1), 2)
        } else {
          new BitmapIndexedSetNode[A](dataMap, 0, Array(key1, key0), Array(originalKeyHash1, originalKeyHash0), 2)
        }
      } else {
        // identical prefixes, payload must be disambiguated deeper in the trie
        val nodeMap = bitposFrom(mask0)
        val node = mergeTwoKeyValPairs(key0, originalKeyHash0, keyHash0, key1, originalKeyHash1, keyHash1, shift + BitPartitionSize)

        new BitmapIndexedSetNode[A](0, nodeMap, Array(node), Array(), node.size)
      }
    }
  }

  def sizePredicate: Int =
    if (nodeArity == 0) payloadArity match {
      case 0 => SizeEmpty
      case 1 => SizeOne
      case _ => SizeMoreThanOne
    } else SizeMoreThanOne

  def hasPayload: Boolean = dataMap != 0

  def payloadArity: Int = bitCount(dataMap)

  def hasNodes: Boolean = nodeMap != 0

  def nodeArity: Int = bitCount(nodeMap)

  def dataIndex(bitpos: Int) = bitCount(dataMap & (bitpos - 1))

  def nodeIndex(bitpos: Int) = bitCount(nodeMap & (bitpos - 1))

  def copyAndSetNode(bitpos: Int, oldNode: SetNode[A], newNode: SetNode[A]) = {
    val idx = this.content.length - 1 - this.nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    dst(idx) = newNode
    new BitmapIndexedSetNode[A](dataMap, nodeMap, dst, originalHashes, size - oldNode.size + newNode.size)
  }

  def copyAndInsertValue(bitpos: Int, key: A, originalHash: Int, elementHash: Int) = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length + 1)

    // copy 'src' and insert 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    dst(idx) = key
    arraycopy(src, idx, dst, idx + 1, src.length - idx)
    val dstHashes = insertElement(originalHashes, dataIx, originalHash)

    new BitmapIndexedSetNode[A](dataMap | bitpos, nodeMap, dst, dstHashes, size + 1)
  }

  def copyAndSetValue(bitpos: Int, key: A, originalHash: Int, elementHash: Int) = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    dst(idx) = key

    new BitmapIndexedSetNode[A](dataMap | bitpos, nodeMap, dst, originalHashes, size)
  }

  def copyAndRemoveValue(bitpos: Int) = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length - 1)

    // copy 'src' and remove 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    arraycopy(src, idx + 1, dst, idx, src.length - idx - 1)
    val dstHashes = removeElement(originalHashes, dataIx)
    new BitmapIndexedSetNode[A](dataMap ^ bitpos, nodeMap, dst, dstHashes, size - 1)
  }

  def copyAndMigrateFromInlineToNode(bitpos: Int, node: SetNode[A]) = {
    val dataIx = dataIndex(bitpos)
    val idxOld = TupleLength * dataIx
    val idxNew = this.content.length - TupleLength - nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length - 1 + 1)

    // copy 'src' and remove 1 element(s) at position 'idxOld' and
    // insert 1 element(s) at position 'idxNew'
    // assert(idxOld <= idxNew)
    arraycopy(src, 0, dst, 0, idxOld)
    arraycopy(src, idxOld + 1, dst, idxOld, idxNew - idxOld)
    dst(idxNew) = node
    arraycopy(src, idxNew + 1, dst, idxNew + 1, src.length - idxNew - 1)
    val dstHashes = removeElement(originalHashes, dataIx)
    new BitmapIndexedSetNode[A](dataMap ^ bitpos, nodeMap | bitpos, dst, dstHashes, size - 1 + node.size)
  }

  def copyAndMigrateFromNodeToInline(bitpos: Int, elementHash: Int, oldNode: SetNode[A], node: SetNode[A]) = {
    val idxOld = this.content.length - 1 - nodeIndex(bitpos)
    val dataIxNew = dataIndex(bitpos)
    val idxNew = TupleLength * dataIxNew

    val src = this.content
    val dst = new Array[Any](src.length - 1 + 1)

    // copy 'src' and remove 1 element(s) at position 'idxOld' and
    // insert 1 element(s) at position 'idxNew'
    // assert(idxOld >= idxNew)
    arraycopy(src, 0, dst, 0, idxNew)
    dst(idxNew) = node.getPayload(0)
    arraycopy(src, idxNew, dst, idxNew + 1, idxOld - idxNew)
    arraycopy(src, idxOld + 1, dst, idxOld + 1, src.length - idxOld - 1)
    val hash = node.getHash(0)
    val dstHashes = insertElement(originalHashes, dataIxNew, hash)
    new BitmapIndexedSetNode[A](dataMap | bitpos, nodeMap ^ bitpos, dst, dstHashes, size - oldNode.size + 1)
  }

  def foreach[U](f: A => U): Unit = {
    var i = 0
    while (i < payloadArity) {
      f(getPayload(i))
      i += 1
    }

    var j = 0
    while (j < nodeArity) {
      getNode(j).foreach(f)
      j += 1
    }
  }

  def subsetOf(that: SetNode[A], shift: Int): Boolean = if (this eq that) true else that match {
    case node: HashCollisionSetNode[A] => false
    case node: BitmapIndexedSetNode[A] => {
      val thisBitmap = this.dataMap | this.nodeMap
      val nodeBitmap = node.dataMap | node.nodeMap

      if ((thisBitmap | nodeBitmap) != nodeBitmap)
        return false

      var bitmap = thisBitmap & nodeBitmap
      var bitsToSkip = numberOfTrailingZeros(bitmap)

      var isValidSubset = true
      while (isValidSubset && bitsToSkip < HashCodeLength) {
        val bitpos = bitposFrom(bitsToSkip)

        isValidSubset =
          if ((this.dataMap & bitpos) != 0) {
            if ((node.dataMap & bitpos) != 0) {
              // Data x Data
              val payload0 = this.getPayload(indexFrom(this.dataMap, bitpos))
              val payload1 = node.getPayload(indexFrom(node.dataMap, bitpos))
              payload0 == payload1
            } else {
              // Data x Node
              val payload = this.getPayload(indexFrom(this.dataMap, bitpos))
              val subNode = that.getNode(indexFrom(node.nodeMap, bitpos))
              val elementUnimprovedHash = payload.##
              val elementHash = improve(elementUnimprovedHash)
              subNode.contains(payload, elementUnimprovedHash, elementHash, shift + BitPartitionSize)
            }
          } else {
            // Node x Node
            val subNode0 = this.getNode(indexFrom(this.nodeMap, bitpos))
            val subNode1 = node.getNode(indexFrom(node.nodeMap, bitpos))
            subNode0.subsetOf(subNode1, shift + BitPartitionSize)
          }

        val newBitmap = bitmap ^ bitpos
        bitmap = newBitmap
        bitsToSkip = numberOfTrailingZeros(newBitmap)
      }
      isValidSubset
    }
  }

  override def equals(that: Any): Boolean =
    that match {
      case node: BitmapIndexedSetNode[A] =>
        (this eq node) ||
          (this.nodeMap == node.nodeMap) &&
            (this.dataMap == node.dataMap) &&
             java.util.Arrays.equals(this.originalHashes, node.originalHashes) &&
              deepContentEquality(this.content, node.content, content.length)
      case _ => false
    }

  @`inline` private def deepContentEquality(a1: Array[Any], a2: Array[Any], length: Int): Boolean = {
    if (a1 eq a2)
      true
    else {
      var isEqual = true
      var i = 0

      while (isEqual && i < length) {
        isEqual = a1(i) == a2(i)
        i += 1
      }

      isEqual
    }
  }

  override def hashCode(): Int =
    throw new UnsupportedOperationException("Trie nodes do not support hashing.")

}

private final class HashCollisionSetNode[A](val originalHash: Int, val hash: Int, var content: Array[Any]) extends SetNode[A] {

  import Node._

  require(content.length >= 2)

  def contains(element: A, originalHash: Int, hash: Int, shift: Int): Boolean =
    this.hash == hash && content.contains(element)

  def updated(element: A, originalHash: Int, hash: Int, shift: Int): SetNode[A] =
    if (this.contains(element, originalHash, hash, shift)) {
      this
    } else {
      new HashCollisionSetNode[A](originalHash, hash, content.appended(element))
    }

  /**
    * Remove an element from the hash collision node.
    *
    * When after deletion only one element remains, we return a bit-mapped indexed node with a
    * singleton element and a hash-prefix for trie level 0. This node will be then a) either become
    * the new root, or b) unwrapped and inlined deeper in the trie.
    */
  def removed(element: A, originalHash: Int, hash: Int, shift: Int): SetNode[A] =
    if (!this.contains(element, originalHash, hash, shift)) {
      this
    } else {
      val updatedContent = content.filterNot(element0 => element0 == element)
      // assert(updatedContent.size == content.size - 1)

      updatedContent.size match {
        case 1 => new BitmapIndexedSetNode[A](bitposFrom(maskFrom(hash, 0)), 0, updatedContent, Array(originalHash), 1)
        case _ => new HashCollisionSetNode[A](originalHash, hash, updatedContent)
      }
    }

  def hasNodes: Boolean = false

  def nodeArity: Int = 0

  def getNode(index: Int): SetNode[A] =
    throw new IndexOutOfBoundsException("No sub-nodes present in hash-collision leaf node.")

  def hasPayload: Boolean = true

  def payloadArity: Int = content.length

  def getPayload(index: Int): A = content(index).asInstanceOf[A]

  override def getHash(index: Int): Int = originalHash

  def sizePredicate: Int = SizeMoreThanOne

  def size: Int = content.length

  def foreach[U](f: A => U): Unit = {
    var i = 0
    while (i < content.length) {
      f(getPayload(i))
      i += 1
    }
  }

  def subsetOf(that: SetNode[A], shift: Int): Boolean = if (this eq that) true else that match {
    case node: BitmapIndexedSetNode[A] => false
    case node: HashCollisionSetNode[A] =>
      this.payloadArity <= node.payloadArity && this.content.forall(node.content.contains)
  }

  override def equals(that: Any): Boolean =
    that match {
      case node: HashCollisionSetNode[A] =>
        (this eq node) ||
          (this.hash == node.hash) &&
            (this.content.size == node.content.size) &&
            (this.content.forall(node.content.contains))
      case _ => false
    }

  override def hashCode(): Int =
    throw new UnsupportedOperationException("Trie nodes do not support hashing.")

}

private final class SetIterator[A](rootNode: SetNode[A])
  extends ChampBaseIterator[SetNode[A]](rootNode) with Iterator[A] {

  def next() = {
    if (!hasNext)
      throw new NoSuchElementException

    val payload = currentValueNode.getPayload(currentValueCursor)
    currentValueCursor += 1

    payload
  }

}

private final class SetReverseIterator[A](rootNode: SetNode[A])
  extends ChampBaseReverseIterator[SetNode[A]](rootNode) with Iterator[A] {

  def next() = {
    if (!hasNext)
      throw new NoSuchElementException

    val payload = currentValueNode.getPayload(currentValueCursor)
    currentValueCursor -= 1

    payload
  }

}

private final class SetHashIterator[A](rootNode: SetNode[A])
  extends ChampBaseIterator[SetNode[A]](rootNode) with Iterator[AnyRef] {
  private[this] var hash = 0
  override def hashCode(): Int = hash

  def next(): AnyRef = {
    if (!hasNext)
      throw new NoSuchElementException

    hash = currentValueNode.getHash(currentValueCursor)
    currentValueCursor += 1
    this
  }

}


/**
  * $factoryInfo
  *
  * @define Coll `immutable.HashSet`
  * @define coll immutable champ hash set
  */
@SerialVersionUID(3L)
object HashSet extends IterableFactory[HashSet] {

  private[HashSet] def apply[A](rootNode: SetNode[A], cachedJavaHashCode: Int) =
    new HashSet[A](rootNode, cachedJavaHashCode)

  private final val EmptySet = new HashSet(SetNode.empty, 0)

  def empty[A]: HashSet[A] =
    EmptySet.asInstanceOf[HashSet[A]]

  def from[A](source: collection.IterableOnce[A]): HashSet[A] =
    source match {
      case hs: HashSet[A] => hs
      case _ if source.knownSize == 0 => empty[A]
      case _ => (newBuilder[A] ++= source).result()
    }

  def newBuilder[A]: Builder[A, HashSet[A]] = new HashSetBuilder

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}

private[collection] final class HashSetBuilder[A] extends Builder[A, HashSet[A]] {
  import Node._
  import SetNode._

  private def newEmptyRootNode = new BitmapIndexedSetNode[A](0, 0, Array(), Array(), 0)

  /** True if and only if `rootNode` has been given out as a return value of `result()`.
    * Indicates that on next add, the elements should be copied to an identical structure, before continuing
    * mutations. */
  private var aliased: Boolean = false

  /** The root node of the partially build hashmap */
  private var rootNode: SetNode[A] = newEmptyRootNode

  /** The cached hash of the partially-built hashmap */
  private var hash: Int = 0

  /** Inserts element `elem` into array `as` at index `ix`, shifting right the trailing elems */
  private def insertElement(as: Array[Int], ix: Int, elem: Int): Array[Int] = {
    if (ix < 0) throw new ArrayIndexOutOfBoundsException
    if (ix > as.length) throw new ArrayIndexOutOfBoundsException
    val result = new Array[Int](as.length + 1)
    arraycopy(as, 0, result, 0, ix)
    result(ix) = elem
    arraycopy(as, ix, result, ix + 1, as.length - ix)
    result
  }

  /** Inserts key-value into the bitmapIndexMapNode. Requires that this is a new key-value pair */
  private def insertValue[A1 >: A](bm: BitmapIndexedSetNode[A],bitpos: Int, key: A, originalHash: Int, keyHash: Int): Unit = {
    val dataIx = bm.dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = bm.content
    val dst = new Array[Any](src.length + TupleLength)

    // copy 'src' and insert 2 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    dst(idx) = key
    arraycopy(src, idx, dst, idx + TupleLength, src.length - idx)

    val dstHashes = insertElement(bm.originalHashes, dataIx, originalHash)

    bm.dataMap = bm.dataMap | bitpos
    bm.content = dst
    bm.originalHashes = dstHashes
    bm.size += 1
  }

  /** Removes element at index `ix` from array `as`, shifting the trailing elements right */
  private def removeElement(as: Array[Int], ix: Int): Array[Int] = {
    if (ix < 0) throw new ArrayIndexOutOfBoundsException
    if (ix > as.length - 1) throw new ArrayIndexOutOfBoundsException
    val result = new Array[Int](as.length - 1)
    arraycopy(as, 0, result, 0, ix)
    arraycopy(as, ix + 1, result, ix, as.length - ix - 1)
    result
  }

  /** Mutates `bm` to replace inline data at bit position `bitpos` with node `node` */
  private def migrateFromInlineToNode(bm: BitmapIndexedSetNode[A], bitpos: Int, node: SetNode[A]): Unit = {
    val dataIx = bm.dataIndex(bitpos)
    val idxOld = TupleLength * dataIx
    val idxNew = bm.content.length - TupleLength - bm.nodeIndex(bitpos)

    val src = bm.content
    val dst = new Array[Any](src.length - TupleLength + 1)

    // copy 'src' and remove 2 element(s) at position 'idxOld' and
    // insert 1 element(s) at position 'idxNew'
    // assert(idxOld <= idxNew)
    arraycopy(src, 0, dst, 0, idxOld)
    arraycopy(src, idxOld + TupleLength, dst, idxOld, idxNew - idxOld)
    dst(idxNew) = node
    arraycopy(src, idxNew + TupleLength, dst, idxNew + 1, src.length - idxNew - TupleLength)

    val dstHashes = removeElement(bm.originalHashes, dataIx)

    bm.dataMap = bm.dataMap ^ bitpos
    bm.nodeMap = bm.nodeMap | bitpos
    bm.content = dst
    bm.originalHashes = dstHashes
    bm.size = bm.size - 1 + node.size
  }

  /** Mutates `bm` to replace inline data at bit position `bitpos` with updated key/value */
  private def setValue[A1 >: A](bm: BitmapIndexedSetNode[A], bitpos: Int, elem: A, originalHash: Int, elementHas: Int): Unit = {
    val dataIx = bm.dataIndex(bitpos)
    val idx = TupleLength * dataIx
    bm.dataMap |= bitpos
    bm.content(idx) = elem
  }

  def update(setNode: SetNode[A], element: A, originalHash: Int, elementHash: Int, shift: Int): Unit =
    setNode match {
      case bm: BitmapIndexedSetNode[A] =>
        val mask = maskFrom(elementHash, shift)
        val bitpos = bitposFrom(mask)

        if ((bm.dataMap & bitpos) != 0) {
          val index = indexFrom(bm.dataMap, mask, bitpos)
          val element0 = bm.getPayload(index)

          if (element0.asInstanceOf[AnyRef] eq element.asInstanceOf[AnyRef]) {
            () // do nothing, element already in set
          } else {
            if (element0.equals(element)) {
              val element0UnimprovedHash = element0.##
              val element0Hash = improve(element0UnimprovedHash)
              setValue(bm, bitpos, element, originalHash, elementHash)
            } else {
              val element0UnimprovedHash = element0.##
              val element0Hash = improve(element0UnimprovedHash)
              val subNodeNew = bm.mergeTwoKeyValPairs(element0, element0UnimprovedHash, element0Hash, element, originalHash, elementHash, shift + BitPartitionSize)
              migrateFromInlineToNode(bm, bitpos, subNodeNew)
              hash += elementHash
            }
          }
        } else if ((bm.nodeMap & bitpos) != 0) {
          val index = indexFrom(bm.nodeMap, mask, bitpos)
          val subNode = bm.getNode(index)
          val beforeSize = subNode.size
          update(subNode, element, originalHash, elementHash, shift + BitPartitionSize)
          bm.size += subNode.size - beforeSize
        } else {
          insertValue(bm, bitpos, element, originalHash, elementHash)
          hash += elementHash
        }
    case hc: HashCollisionSetNode[A] =>
      val index = hc.content.indexOf(element)
      if (index < 0) {
        hash += elementHash
        hc.content = hc.content.appended(element)
      } else {
        hc.content(index) = element
      }
  }


  /** Inserts the this key/value only if the key is not present in the map already */
//  private def updateIfNotExists(setNode: SetNode[A], element: A, originalHash: Int, elementHash: Int, shift: Int) = {
  private def updateIfNotExists(setNode: SetNode[A], element: A, originalHash: Int, elementHash: Int, shift: Int): Unit = {
    setNode match {
      case bm: BitmapIndexedSetNode[A] =>
        val mask = maskFrom(elementHash, shift)
        val bitpos = bitposFrom(mask)
        if ((bm.dataMap & bitpos) != 0) {
          val index = indexFrom(bm.dataMap, mask, bitpos)
          val element0 = bm.getPayload(index)
          if (element0 != element) {
            val element0UnimprovedHash = element0.##
            val element0Hash = improve(element0UnimprovedHash)
            val subNodeNew = bm.mergeTwoKeyValPairs(element0, element0UnimprovedHash, element0Hash, element, originalHash, elementHash, shift + BitPartitionSize)
            migrateFromInlineToNode(bm, bitpos, subNodeNew)
            hash += elementHash
          }
        } else if ((bm.nodeMap & bitpos) != 0) {
          val index = indexFrom(bm.nodeMap, mask, bitpos)
          val subNode = bm.getNode(index)
          val beforeSize = subNode.size
          updateIfNotExists(subNode, element, originalHash, elementHash, shift + BitPartitionSize)
          bm.size += subNode.size - beforeSize
        } else {
          insertValue(bm, bitpos, element, originalHash, elementHash)
          hash += elementHash
        }
      case hc: HashCollisionSetNode[A] =>
        val index = hc.content.indexOf(element)
        if (index < 0) {
          hash += elementHash
          hc.content = hc.content.appended(element)
        }
    }
  }


  /** If currently referencing aliased structure, copy elements to new mutable structure */
  private def ensureUnaliased():Unit = {
    if (aliased) copyElems()
    aliased = false
  }

  /** Copy elements to new mutable structure */
  private def copyElems(): Unit = {
    val temp = rootNode
    clear()
    temp.foreach(addOne)
  }

  override def result(): HashSet[A] =
    if (rootNode.size == 0) {
      HashSet.empty
    } else {
      aliased = true
      val hm = new HashSet(rootNode, hash)
      releaseFence()
      hm
    }

  override def addOne(elem: A): this.type = {
    ensureUnaliased()
    val h = elem.##
    val im = improve(h)
    update(rootNode, elem, h, im, 0)
    this
  }


  private[collection] def addOneIfNotExists(elem: A): this.type = {
    ensureUnaliased()
    val h = elem.##
    val im = improve(h)
    updateIfNotExists(rootNode, elem, h, im, 0)
    this
  }

  override def addAll(xs: IterableOnce[A]) = {
    ensureUnaliased()
    xs match {
      case hm: HashSet[A] =>
        def addAllRec(node: SetNode[A]): Unit = node match {
          case bm: BitmapIndexedSetNode[A] =>
            var i = 0
            val payloadArity = bm.payloadArity
            while (i < payloadArity) {
              val originalHash = bm.getHash(i)
              update(rootNode, bm.getPayload(i), originalHash, improve(originalHash), 0)
              i += 1
            }

            var j = 0
            while (j < bm.nodeArity) {
              addAllRec(bm.getNode(j))
              j += 1
            }

          case hc: HashCollisionSetNode[A] =>
            var i = 0
            while (i < hc.content.length) {
              update(rootNode, hc.getPayload(i), hc.originalHash, hc.hash, shift = 0)
              i += 1
            }
        }
        addAllRec(hm.rootNode)
      case other =>
        val it = other.iterator
        while(it.hasNext) addOne(it.next())
    }

    this
  }

  override def clear(): Unit = {
    aliased = false
    if (rootNode.size > 0) {
      // if rootNode is empty, we will not have given it away anyways, we instead give out the reused Set.empty
      rootNode = new BitmapIndexedSetNode[A](0, 0, Array(), Array(), 0)
    }
    hash = 0
  }

  private[collection] def size: Int = rootNode.size
}

