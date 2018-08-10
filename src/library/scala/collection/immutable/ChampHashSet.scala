package scala
package collection
package immutable

import java.io.{ObjectInputStream, ObjectOutputStream}

import mutable.{Builder, ImmutableBuilder}
import Hashing.computeHash
import java.lang.Integer.{bitCount, numberOfTrailingZeros}
import java.lang.System.arraycopy

/** This class implements immutable sets using a Compressed Hash-Array Mapped Prefix-tree.
  * See paper https://michael.steindorfer.name/publications/oopsla15.pdf for more details.
  *
  *  @tparam A      the type of the elements contained in this hash set.
  *
  *  @author  Michael J. Steindorfer
  *  @since   2.13
  *  @define Coll `immutable.ChampHashSet`
  *  @define coll immutable champ hash set
  */
final class ChampHashSet[A] private[immutable] (val rootNode: SetNode[A], val cachedJavaHashCode: Int, val cachedSize: Int)
  extends AbstractSet[A]
    with SetOps[A, ChampHashSet, ChampHashSet[A]]
    with StrictOptimizedIterableOps[A, ChampHashSet, ChampHashSet[A]] {

  override def iterableFactory: IterableFactory[ChampHashSet] = ChampHashSet

  override def knownSize: Int = cachedSize

  override def size: Int = cachedSize

  override def isEmpty: Boolean = cachedSize == 0

  def iterator: Iterator[A] = {
    if (isEmpty) Iterator.empty
    else new SetIterator[A](rootNode)
  }

  protected[immutable] def reverseIterator: Iterator[A] = new SetReverseIterator[A](rootNode)

  def contains(element: A): Boolean = rootNode.contains(element, computeHash(element), 0)

  def incl(element: A): ChampHashSet[A] = {
    val elementHash = computeHash(element)
    val newRootNode = rootNode.updated(element, elementHash, 0)

    if (newRootNode ne rootNode)
      ChampHashSet(newRootNode, cachedJavaHashCode + elementHash, cachedSize + 1)
    else this
  }

  def excl(element: A): ChampHashSet[A] = {
    val elementHash = computeHash(element)
    val newRootNode = rootNode.removed(element, elementHash, 0)

    if (rootNode ne newRootNode)
      ChampHashSet(newRootNode, cachedJavaHashCode - elementHash, cachedSize - 1)
    else this
  }

  override def tail: ChampHashSet[A] = this - head

  override def init: ChampHashSet[A] = this - last

  override def head: A = iterator.next()

  override def last: A = reverseIterator.next()

  override def foreach[U](f: A => U): Unit = rootNode.foreach(f)

  def subsetOf(that: Set[A]): Boolean = if (that.isEmpty) true else that match {
    case set: ChampHashSet[A] => rootNode.subsetOf(set.rootNode, 0)
    case _ => super.subsetOf(that)
  }

  override def equals(that: Any): Boolean =
    that match {
      case set: ChampHashSet[A] =>
        (this eq set) ||
          (this.cachedSize == set.cachedSize) &&
            (this.cachedJavaHashCode == set.cachedJavaHashCode) &&
            (this.rootNode == set.rootNode)
      case _ => super.equals(that)
    }

  override protected[this] def className = "ChampHashSet"
}

private[immutable] final object SetNode {

  private final val EmptySetNode = new BitmapIndexedSetNode(0, 0, Array.empty, Array.empty)

  def empty[A]: SetNode[A] =
    EmptySetNode.asInstanceOf[SetNode[A]]

  final val TupleLength = 1

}

private[immutable] sealed abstract class SetNode[A] extends Node[SetNode[A]] {

  def contains(element: A, hash: Int, shift: Int): Boolean

  def updated(element: A, hash: Int, shift: Int): SetNode[A]

  def removed(element: A, hash: Int, shift: Int): SetNode[A]

  def hasNodes: Boolean

  def nodeArity: Int

  def getNode(index: Int): SetNode[A]

  def hasPayload: Boolean

  def payloadArity: Int

  def getPayload(index: Int): A

  def sizePredicate: Int

  def foreach[U](f: A => U): Unit

  def subsetOf(that: SetNode[A], shift: Int): Boolean

}

private final class BitmapIndexedSetNode[A](val dataMap: Int, val nodeMap: Int, val content: Array[Any], val hashes: Array[Int]) extends SetNode[A] {

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

  def getNode(index: Int) =
    content(content.length - 1 - index).asInstanceOf[SetNode[A]]

  def contains(element: A, elementHash: Int, shift: Int): Boolean = {
    val mask = maskFrom(elementHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      return hashes(index) == elementHash && element == this.getPayload(index)
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      return this.getNode(index).contains(element, elementHash, shift + BitPartitionSize)
    }

    false
  }

  def updated(element: A, elementHash: Int, shift: Int): SetNode[A] = {
    val mask = maskFrom(elementHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val element0 = this.getPayload(index)

      if (element0 == element) {
        return this
      } else {
        val subNodeNew = mergeTwoKeyValPairs(element0, computeHash(element0), element, elementHash, shift + BitPartitionSize)
        return copyAndMigrateFromInlineToNode(bitpos, subNodeNew)
      }
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.updated(element, elementHash, shift + BitPartitionSize)
      if (subNode eq subNodeNew) {
        return this
      } else {
        return copyAndSetNode(bitpos, subNodeNew)
      }
    }

    copyAndInsertValue(bitpos, element, elementHash)
  }

  def removed(element: A, elementHash: Int, shift: Int): SetNode[A] = {
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
            return new BitmapIndexedSetNode[A](newDataMap, 0, Array(getPayload(1)), Array(hashes(1)))
          else
            return new BitmapIndexedSetNode[A](newDataMap, 0, Array(getPayload(0)), Array(hashes(0)))
        }
        else return copyAndRemoveValue(bitpos)
      } else return this
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.removed(element, elementHash, shift + BitPartitionSize)
      // assert(subNodeNew.sizePredicate != SizeEmpty, "Sub-node must have at least one element.")

      if (subNodeNew eq subNode) return this
      subNodeNew.sizePredicate match {
        case SizeOne =>
          if (this.payloadArity == 0 && this.nodeArity == 1) { // escalate (singleton or empty) result
            return subNodeNew
          }
          else { // inline value (move to front)
            return copyAndMigrateFromNodeToInline(bitpos, elementHash, subNodeNew)
          }

        case SizeMoreThanOne =>
          // modify current node (set replacement node)
          return copyAndSetNode(bitpos, subNodeNew)
      }
    }

    this
  }

  def mergeTwoKeyValPairs(key0: A, keyHash0: Int, key1: A, keyHash1: Int, shift: Int): SetNode[A] = {
    // assert(key0 != key1)

    if (shift >= HashCodeLength) {
      new HashCollisionSetNode[A](keyHash0, Vector(key0, key1))
    } else {
      val mask0 = maskFrom(keyHash0, shift)
      val mask1 = maskFrom(keyHash1, shift)

      if (mask0 != mask1) {
        // unique prefixes, payload fits on same level
        val dataMap = bitposFrom(mask0) | bitposFrom(mask1)

        if (mask0 < mask1) {
          new BitmapIndexedSetNode[A](dataMap, 0, Array(key0, key1), Array(keyHash0, keyHash1))
        } else {
          new BitmapIndexedSetNode[A](dataMap, 0, Array(key1, key0), Array(keyHash1, keyHash0))
        }
      } else {
        // identical prefixes, payload must be disambiguated deeper in the trie
        val nodeMap = bitposFrom(mask0)
        val node = mergeTwoKeyValPairs(key0, keyHash0, key1, keyHash1, shift + BitPartitionSize)

        new BitmapIndexedSetNode[A](0, nodeMap, Array(node), Array())
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

  def copyAndSetNode(bitpos: Int, newNode: SetNode[A]) = {
    val idx = this.content.length - 1 - this.nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    dst(idx) = newNode
    new BitmapIndexedSetNode[A](dataMap, nodeMap, dst, hashes)
  }

  def copyAndInsertValue(bitpos: Int, key: A, elementHash: Int) = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length + 1)

    // copy 'src' and insert 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    dst(idx) = key
    arraycopy(src, idx, dst, idx + 1, src.length - idx)
    val dstHashes = insertElement(hashes, dataIx, elementHash)

    new BitmapIndexedSetNode[A](dataMap | bitpos, nodeMap, dst, dstHashes)
  }

  def copyAndRemoveValue(bitpos: Int) = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length - 1)

    // copy 'src' and remove 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    arraycopy(src, idx + 1, dst, idx, src.length - idx - 1)
    val dstHashes = removeElement(hashes, dataIx)
    new BitmapIndexedSetNode[A](dataMap ^ bitpos, nodeMap, dst, dstHashes)
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
    val dstHashes = removeElement(hashes, dataIx)
    new BitmapIndexedSetNode[A](dataMap ^ bitpos, nodeMap | bitpos, dst, dstHashes)
  }

  def copyAndMigrateFromNodeToInline(bitpos: Int, elementHash: Int, node: SetNode[A]) = {
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
    val hash = node match {
      case bitnode: BitmapIndexedSetNode[_] => bitnode.hashes(0)
      case collision: HashCollisionSetNode[_] => collision.hash
    }
    val dstHashes = insertElement(hashes, dataIxNew, hash)
    new BitmapIndexedSetNode[A](dataMap | bitpos, nodeMap ^ bitpos, dst, dstHashes)
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
              subNode.contains(payload, computeHash(payload), shift + BitPartitionSize);
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

private final class HashCollisionSetNode[A](val hash: Int, val content: Vector[A]) extends SetNode[A] {

  import Node._

  require(content.size >= 2)

  def contains(element: A, hash: Int, shift: Int): Boolean =
    this.hash == hash && content.contains(element)

  def updated(element: A, hash: Int, shift: Int): SetNode[A] =
    if (this.contains(element, hash, shift)) {
      this
    } else {
      new HashCollisionSetNode[A](hash, content.appended(element))
    }

  /**
    * Remove an element from the hash collision node.
    *
    * When after deletion only one element remains, we return a bit-mapped indexed node with a
    * singleton element and a hash-prefix for trie level 0. This node will be then a) either become
    * the new root, or b) unwrapped and inlined deeper in the trie.
    */
  def removed(element: A, hash: Int, shift: Int): SetNode[A] =
    if (!this.contains(element, hash, shift)) {
      this
    } else {
      val updatedContent = content.filterNot(element0 => element0 == element)
      // assert(updatedContent.size == content.size - 1)

      updatedContent.size match {
        case 1 => new BitmapIndexedSetNode[A](bitposFrom(maskFrom(hash, 0)), 0, updatedContent.toArray, Array(hash))
        case _ => new HashCollisionSetNode[A](hash, updatedContent)
      }
    }

  def hasNodes: Boolean = false

  def nodeArity: Int = 0

  def getNode(index: Int): SetNode[A] =
    throw new IndexOutOfBoundsException("No sub-nodes present in hash-collision leaf node.")

  def hasPayload: Boolean = true

  def payloadArity: Int = content.size

  def getPayload(index: Int): A = content(index)

  def sizePredicate: Int = SizeMoreThanOne

  def foreach[U](f: A => U): Unit = content.foreach(f)

  def subsetOf(that: SetNode[A], shift: Int): Boolean = if (this eq that) true else that match {
    case node: BitmapIndexedSetNode[A] => false
    case node: HashCollisionSetNode[A] => {
      this.payloadArity <= node.payloadArity && this.content.forall(node.content.contains)
    }
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

/**
  * $factoryInfo
  *
  * @define Coll `immutable.ChampHashSet`
  * @define coll immutable champ hash set
  */
@SerialVersionUID(3L)
object ChampHashSet extends IterableFactory[ChampHashSet] {

  private[ChampHashSet] def apply[A](rootNode: SetNode[A], cachedJavaHashCode: Int, cachedSize: Int) =
    new ChampHashSet[A](rootNode, cachedJavaHashCode, cachedSize)

  private final val EmptySet = new ChampHashSet(SetNode.empty, 0, 0)

  def empty[A]: ChampHashSet[A] =
    EmptySet.asInstanceOf[ChampHashSet[A]]

  def from[A](source: collection.IterableOnce[A]): ChampHashSet[A] =
    source match {
      case hs: ChampHashSet[A] => hs
      case _ if source.knownSize == 0 => empty[A]
      case _ => (newBuilder[A] ++= source).result()
    }

  def newBuilder[A]: Builder[A, ChampHashSet[A]] =
    new ImmutableBuilder[A, ChampHashSet[A]](empty) {
      def addOne(element: A): this.type = {
        elems = elems + element
        this
      }
    }

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}
