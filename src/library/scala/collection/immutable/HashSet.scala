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

package scala
package collection
package immutable

import mutable.ReusableBuilder
import Hashing.improve
import java.lang.Integer.{bitCount, numberOfTrailingZeros}
import java.lang.System.arraycopy

import scala.collection.generic.DefaultSerializable
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
final class HashSet[A] private[immutable] (val rootNode: SetNode[A])
  extends AbstractSet[A]
    with StrictOptimizedSetOps[A, HashSet, HashSet[A]]
    with DefaultSerializable {

  releaseFence()

  private[this] def newHashSetOrThis(newRootNode: SetNode[A]): HashSet[A] =
    if (rootNode eq newRootNode) this else new HashSet(newRootNode)

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
    newHashSetOrThis(newRootNode)
  }

  def excl(element: A): HashSet[A] = {
    val elementUnimprovedHash = element.##
    val elementHash = improve(elementUnimprovedHash)
    val newRootNode = rootNode.removed(element, elementUnimprovedHash, elementHash, 0)
    newHashSetOrThis(newRootNode)
  }

  override def concat(that: IterableOnce[A])(implicit dummy: DummyImplicit): HashSet[A] =
    that match {
      case hs: HashSet[A] => newHashSetOrThis(rootNode.concat(hs.rootNode, 0))
      case _ => super.concat(that)
    }

  override def tail: HashSet[A] = this - head

  override def init: HashSet[A] = this - last

  override def head: A = iterator.next()

  override def last: A = reverseIterator.next()

  override def foreach[U](f: A => U): Unit = rootNode.foreach(f)

  /** Applies a function f to each element, and its corresponding **original** hash, in this Set */
  @`inline` private[collection] def foreachWithHash(f: (A, Int) => Unit): Unit = rootNode.foreachWithHash(f)

  /** Applies a function f to each element, and its corresponding **original** hash, in this Set
    * Stops iterating the first time that f returns `false`.*/
  @`inline` private[collection] def foreachWithHashWhile(f: (A, Int) => Boolean): Unit = rootNode.foreachWithHashWhile(f)

  def subsetOf(that: Set[A]): Boolean = if (that.isEmpty) true else that match {
    case set: HashSet[A] => rootNode.subsetOf(set.rootNode, 0)
    case _ => super.subsetOf(that)
  }

  override def equals(that: Any): Boolean =
    that match {
      case set: HashSet[A] => (this eq set) || (this.rootNode == set.rootNode)
      case _ => super.equals(that)
    }

  override protected[this] def className = "HashSet"

  override def hashCode(): Int = {
    val it = new SetHashIterator(rootNode)
    val hash = MurmurHash3.unorderedHash(it, MurmurHash3.setSeed)
    //assert(hash == super.hashCode())
    hash
  }

  override def diff(that: collection.Set[A]): HashSet[A] = {
    if (isEmpty) {
      this
    } else {
      that match {
        case hashSet: HashSet[A] =>
          if (hashSet.isEmpty) this else {
            val newRootNode = rootNode.diff(hashSet.rootNode, 0)
            if (newRootNode.size == 0) HashSet.empty else newHashSetOrThis(rootNode.diff(hashSet.rootNode, 0))
          }
        case other =>
          val thatKnownSize = other.knownSize

          if (thatKnownSize == 0) {
            this
          } else if (thatKnownSize <= size) {
            /* this branch intentionally includes the case of thatKnownSize == -1. We know that HashSets are quite fast at look-up, so
            we're likely to be the faster of the two at that. */
            val iter = other.iterator
            var curr = rootNode
            while(iter.hasNext) {
              // TODO: add mutable.hashSet#contains(elem, hashCode)
              // in order to avoid rehashing elements
              val next = iter.next()
              val originalHash = next.##
              val improved = improve(originalHash)
              curr = curr.removed(next, originalHash, improved, 0)
              if (curr.size <= 0) {
                return HashSet.empty
              }
            }

            newHashSetOrThis(curr)
          } else {
            // TODO: Develop more sophisticated heuristic for which branch to take
            filterNot(other.contains)
          }
      }

    }
  }

  override def removedAll(that: IterableOnce[A]): HashSet[A] = that match {
    case set: scala.collection.Set[A] => diff(set)
    case range: Range if range.length > size =>
      filter {
        case i: Int => !range.contains(i)
        case _ => true
      }

    case _ =>
      val iter = that.iterator
      var curr = rootNode
      while (iter.hasNext) {
        val next = iter.next()
        val originalHash = next.##
        val improved = improve(originalHash)
        curr = curr.removed(next, originalHash, improved, 0)
        if (curr.size <= 0) {
          return HashSet.empty
        }
      }
      newHashSetOrThis(curr)
  }

  override def partition(p: A => Boolean): (HashSet[A], HashSet[A]) = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.partition(p)
  }

  override def span(p: A => Boolean): (HashSet[A], HashSet[A]) = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.span(p)
  }

  override protected[collection] def filterImpl(pred: A => Boolean, isFlipped: Boolean): HashSet[A] = {
    val newRootNode = rootNode.filterImpl(pred, isFlipped)
    if (newRootNode eq rootNode) this
    else if (newRootNode.size == 0) HashSet.empty
    else new HashSet(newRootNode)
  }

  override def intersect(that: collection.Set[A]): HashSet[A] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.intersect(that)
  }

  override def take(n: Int): HashSet[A] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.take(n)
  }

  override def takeRight(n: Int): HashSet[A] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.takeRight(n)
  }

  override def takeWhile(p: A => Boolean): HashSet[A] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.takeWhile(p)
  }

  override def drop(n: Int): HashSet[A] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.drop(n)
  }

  override def dropRight(n: Int): HashSet[A] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.dropRight(n)
  }

  override def dropWhile(p: A => Boolean): HashSet[A] = {
    // This method has been preemptively overridden in order to ensure that an optimizing implementation may be included
    // in a minor release without breaking binary compatibility.
    super.dropWhile(p)
  }
}

private[immutable] object SetNode {

  private final val EmptySetNode = new BitmapIndexedSetNode(0, 0, Array.empty, Array.empty, 0, 0)

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

  def size: Int

  def foreach[U](f: A => U): Unit

  def subsetOf(that: SetNode[A], shift: Int): Boolean

  def copy(): SetNode[A]

  def filterImpl(pred: A => Boolean, flipped: Boolean): SetNode[A]

  def diff(that: SetNode[A], shift: Int): SetNode[A]

  def concat(that: SetNode[A], shift: Int): SetNode[A]

  def foreachWithHash(f: (A, Int) => Unit): Unit

  def foreachWithHashWhile(f: (A, Int) => Boolean): Boolean
}

private final class BitmapIndexedSetNode[A](
   var dataMap: Int,
   var nodeMap: Int,
   var content: Array[Any],
   var originalHashes: Array[Int],
   var size: Int,
   var cachedJavaKeySetHashCode: Int) extends SetNode[A] {

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

  def getPayload(index: Int): A = content(TupleLength * index).asInstanceOf[A]

  override def getHash(index: Int): Int = originalHashes(index)

  def getNode(index: Int): SetNode[A] = content(content.length - 1 - index).asInstanceOf[SetNode[A]]

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
        val element0UnimprovedHash = getHash(index)
        val element0Hash = improve(element0UnimprovedHash)
        if (originalHash == element0UnimprovedHash && element0.equals(element)) {
          return this
        } else {
          val subNodeNew = mergeTwoKeyValPairs(element0, element0UnimprovedHash, element0Hash, element, originalHash, elementHash, shift + BitPartitionSize)
          return copyAndMigrateFromInlineToNode(bitpos, element0Hash, subNodeNew)
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
            return new BitmapIndexedSetNode[A](newDataMap, 0, Array(getPayload(1)), Array(originalHashes(1)), size - 1, improve(originalHashes(1)))
          else
            return new BitmapIndexedSetNode[A](newDataMap, 0, Array(getPayload(0)), Array(originalHashes(0)), size - 1, improve(originalHashes(0)))
        }
        else return copyAndRemoveValue(bitpos, elementHash)
      } else return this
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.removed(element, originalHash, elementHash, shift + BitPartitionSize)
      // assert(subNodeNew.size != 0, "Sub-node must have at least one element.")

      // cache just in case subNodeNew is a hashCollision node, in which in which case a little arithmetic is avoided
      // in Vector#length
      val subNodeNewSize = subNodeNew.size

      if (subNodeNewSize == 1) {
        if (this.size == subNode.size) {
          // subNode is the only child (no other data or node children of `this` exist)
          // escalate (singleton or empty) result
          return subNodeNew
        } else {
          // inline value (move to front)
          return copyAndMigrateFromNodeToInline(bitpos, elementHash, subNode, subNodeNew)
        }
      } else if (subNodeNewSize > 1) {
        // modify current node (set replacement node)
        return copyAndSetNode(bitpos, subNode, subNodeNew)
      }
    }

    this
  }

  def mergeTwoKeyValPairs(key0: A, originalKeyHash0: Int, keyHash0: Int, key1: A, originalKeyHash1: Int, keyHash1: Int, shift: Int): SetNode[A] = {
    // assert(key0 != key1)

    if (shift >= HashCodeLength) {
      new HashCollisionSetNode[A](originalKeyHash0, keyHash0, Vector(key0, key1))
    } else {
      val mask0 = maskFrom(keyHash0, shift)
      val mask1 = maskFrom(keyHash1, shift)

      if (mask0 != mask1) {
        // unique prefixes, payload fits on same level
        val dataMap = bitposFrom(mask0) | bitposFrom(mask1)
        val newCachedHashCode = keyHash0 + keyHash1

        if (mask0 < mask1) {
          new BitmapIndexedSetNode[A](dataMap, 0, Array(key0, key1), Array(originalKeyHash0, originalKeyHash1), 2, newCachedHashCode)
        } else {
          new BitmapIndexedSetNode[A](dataMap, 0, Array(key1, key0), Array(originalKeyHash1, originalKeyHash0), 2, newCachedHashCode)
        }
      } else {
        // identical prefixes, payload must be disambiguated deeper in the trie
        val nodeMap = bitposFrom(mask0)
        val node = mergeTwoKeyValPairs(key0, originalKeyHash0, keyHash0, key1, originalKeyHash1, keyHash1, shift + BitPartitionSize)

        new BitmapIndexedSetNode[A](0, nodeMap, Array(node), Array(), node.size, node.cachedJavaKeySetHashCode)
      }
    }
  }

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
    new BitmapIndexedSetNode[A](
      dataMap = dataMap,
      nodeMap = nodeMap,
      content = dst,
      originalHashes = originalHashes,
      size = size - oldNode.size + newNode.size,
      cachedJavaKeySetHashCode = cachedJavaKeySetHashCode - oldNode.cachedJavaKeySetHashCode + newNode.cachedJavaKeySetHashCode
    )
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

    new BitmapIndexedSetNode[A](dataMap | bitpos, nodeMap, dst, dstHashes, size + 1, cachedJavaKeySetHashCode + elementHash)
  }

  def copyAndSetValue(bitpos: Int, key: A, originalHash: Int, elementHash: Int) = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    dst(idx) = key

    new BitmapIndexedSetNode[A](dataMap | bitpos, nodeMap, dst, originalHashes, size, cachedJavaKeySetHashCode)
  }

  def copyAndRemoveValue(bitpos: Int, elementHash: Int) = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length - 1)

    // copy 'src' and remove 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    arraycopy(src, idx + 1, dst, idx, src.length - idx - 1)
    val dstHashes = removeElement(originalHashes, dataIx)
    new BitmapIndexedSetNode[A](dataMap ^ bitpos, nodeMap, dst, dstHashes, size - 1, cachedJavaKeySetHashCode - elementHash)
  }

  def copyAndMigrateFromInlineToNode(bitpos: Int, elementHash: Int, node: SetNode[A]) = {
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
    new BitmapIndexedSetNode[A](
      dataMap = dataMap ^ bitpos,
      nodeMap = nodeMap | bitpos,
      content = dst, originalHashes = dstHashes,
      size = size - 1 + node.size,
      cachedJavaKeySetHashCode = cachedJavaKeySetHashCode - elementHash + node.cachedJavaKeySetHashCode
    )
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
    new BitmapIndexedSetNode[A](
      dataMap = dataMap | bitpos,
      nodeMap = nodeMap ^ bitpos,
      content = dst,
      originalHashes = dstHashes,
      size = size - oldNode.size + 1,
      cachedJavaKeySetHashCode = cachedJavaKeySetHashCode - oldNode.cachedJavaKeySetHashCode + node.cachedJavaKeySetHashCode
    )
  }

  def foreach[U](f: A => U): Unit = {
    val thisPayloadArity = payloadArity
    var i = 0
    while (i < thisPayloadArity) {
      f(getPayload(i))
      i += 1
    }

    val thisNodeArity = nodeArity
    var j = 0
    while (j < thisNodeArity) {
      getNode(j).foreach(f)
      j += 1
    }
  }

  def subsetOf(that: SetNode[A], shift: Int): Boolean = if (this eq that) true else that match {
    case _: HashCollisionSetNode[A] => false
    case node: BitmapIndexedSetNode[A] =>
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
              val thisDataIndex = indexFrom(this.dataMap, bitpos)
              val payload = this.getPayload(thisDataIndex)
              val subNode = that.getNode(indexFrom(node.nodeMap, bitpos))
              val elementUnimprovedHash = getHash(thisDataIndex)
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

  override def filterImpl(pred: A => Boolean, flipped: Boolean): SetNode[A] = {
    if (size == 0) this
    else if (size == 1) {
      if (pred(getPayload(0)) != flipped) this else SetNode.empty
    } else {
      val allMap = dataMap | nodeMap
      val minimumIndex: Int = Integer.numberOfTrailingZeros(allMap)
      val maximumIndex: Int = Node.BranchingFactor - Integer.numberOfLeadingZeros(allMap)

      var oldDataPassThrough = 0

      // bitmap of nodes which, when filtered, returned a single-element node. These must be migrated to data
      var nodeMigrateToDataTargetMap = 0

      // TODO: When filtering results in a single-elem node, simply `A` should be returned,
      //  not a singleton (to avoid pointlessly allocating arrays, nodes). This would probably
      //  involve changing the return type of filterImpl to `Any` which may return at runtime a SetNode[A] or an A

      // the queue of single-element, post-filter nodes
      var nodesToMigrateToData: mutable.Queue[SetNode[A]] = null

      // bitmap of all nodes which, when filtered, returned themselves. They are passed forward to the returned node
      var nodesToPassThroughMap = 0

      // bitmap of any nodes which, after being filtered, returned a node that is not empty, but also not `eq` itself
      // These are stored for later inclusion into the final `content` array
      // not named `newNodesMap` (plural) to avoid confusion with `newNodeMap` (singular)
      var mapOfNewNodes = 0
      // each bit in `mapOfNewNodes` corresponds to one element in this queue
      var newNodes: mutable.Queue[SetNode[A]] = null

      var newDataMap = 0
      var newNodeMap = 0
      var newSize = 0
      var newCachedHashCode = 0

      var dataIndex = 0
      var nodeIndex = 0

      var i = minimumIndex
      while (i < maximumIndex) {
        val bitpos = bitposFrom(i)

        if ((bitpos & dataMap) != 0) {
          val payload = getPayload(dataIndex)
          val passed = pred(payload) != flipped

          if (passed) {
            newDataMap |= bitpos
            oldDataPassThrough |= bitpos
            newSize += 1
            newCachedHashCode += improve(getHash(dataIndex))
          }

          dataIndex += 1
        } else if ((bitpos & nodeMap) != 0) {
          val oldSubNode = getNode(nodeIndex)
          val newSubNode = oldSubNode.filterImpl(pred, flipped)

          newSize += newSubNode.size
          newCachedHashCode += newSubNode.cachedJavaKeySetHashCode

          // if (newSubNode.size == 0) do nothing (drop it)
          if (newSubNode.size > 1) {
            newNodeMap |= bitpos
            if (oldSubNode eq newSubNode) {
              nodesToPassThroughMap |= bitpos
            } else {
              mapOfNewNodes |= bitpos
              if (newNodes eq null) {
                newNodes = mutable.Queue.empty
              }
              newNodes += newSubNode
            }
          } else if (newSubNode.size == 1) {
            newDataMap |= bitpos
            nodeMigrateToDataTargetMap |= bitpos
            if (nodesToMigrateToData eq null) {
              nodesToMigrateToData = mutable.Queue.empty
            }
            nodesToMigrateToData += newSubNode
          }

          nodeIndex += 1
        }

        i += 1
      }

      this.newNodeFrom(
        newSize = newSize,
        newDataMap = newDataMap,
        newNodeMap = newNodeMap,
        minimumIndex = minimumIndex,
        oldDataPassThrough = oldDataPassThrough,
        nodesToPassThroughMap = nodesToPassThroughMap,
        nodeMigrateToDataTargetMap = nodeMigrateToDataTargetMap,
        nodesToMigrateToData = nodesToMigrateToData,
        mapOfNewNodes = mapOfNewNodes,
        newNodes = newNodes,
        newCachedHashCode = newCachedHashCode
      )
    }
  }

  override def diff(that: SetNode[A], shift: Int): SetNode[A] = that match {
    case bm: BitmapIndexedSetNode[A] =>
      if (size == 0) this
      else if (size == 1) {
        val h = getHash(0)
        if (that.contains(getPayload(0), h, improve(h), shift)) SetNode.empty else this
      } else {
        val allMap = dataMap | nodeMap
        val minimumIndex: Int = Integer.numberOfTrailingZeros(allMap)
        val maximumIndex: Int = Node.BranchingFactor - Integer.numberOfLeadingZeros(allMap)

        var oldDataPassThrough = 0

        // bitmap of nodes which, when filtered, returned a single-element node. These must be migrated to data
        var nodeMigrateToDataTargetMap = 0
        // the queue of single-element, post-filter nodes
        var nodesToMigrateToData: mutable.Queue[SetNode[A]] = null

        // bitmap of all nodes which, when filtered, returned themselves. They are passed forward to the returned node
        var nodesToPassThroughMap = 0

        // bitmap of any nodes which, after being filtered, returned a node that is not empty, but also not `eq` itself
        // These are stored for later inclusion into the final `content` array
        // not named `newNodesMap` (plural) to avoid confusion with `newNodeMap` (singular)
        var mapOfNewNodes = 0
        // each bit in `mapOfNewNodes` corresponds to one element in this queue
        var newNodes: mutable.Queue[SetNode[A]] = null

        var newDataMap = 0
        var newNodeMap = 0
        var newSize = 0
        var newCachedHashCode = 0

        var dataIndex = 0
        var nodeIndex = 0

        var i = minimumIndex
        while (i < maximumIndex) {
          val bitpos = bitposFrom(i)

          if ((bitpos & dataMap) != 0) {
            val payload = getPayload(dataIndex)
            val originalHash = getHash(dataIndex)
            val hash = improve(originalHash)

            if (!bm.contains(payload, originalHash, hash, shift)) {
              newDataMap |= bitpos
              oldDataPassThrough |= bitpos
              newSize += 1
              newCachedHashCode += hash
            }

            dataIndex += 1
          } else if ((bitpos & nodeMap) != 0) {
            val oldSubNode = getNode(nodeIndex)

            val newSubNode: SetNode[A] =
              if ((bitpos & bm.dataMap) != 0) {
                val thatDataIndex = indexFrom(bm.dataMap, bitpos)
                val thatPayload = bm.getPayload(thatDataIndex)
                val thatOriginalHash = bm.getHash(thatDataIndex)
                val thatHash = improve(thatOriginalHash)
                oldSubNode.removed(thatPayload, thatOriginalHash, thatHash, shift + BitPartitionSize)
              } else if ((bitpos & bm.nodeMap) != 0) {
                oldSubNode.diff(bm.getNode(indexFrom(bm.nodeMap, bitpos)), shift + BitPartitionSize)
              } else {
                oldSubNode
              }

            newSize += newSubNode.size
            newCachedHashCode += newSubNode.cachedJavaKeySetHashCode

            // if (newSubNode.size == 0) do nothing (drop it)
            if (newSubNode.size > 1) {
              newNodeMap |= bitpos
              if (oldSubNode eq newSubNode) {
                nodesToPassThroughMap |= bitpos
              } else {
                mapOfNewNodes |= bitpos
                if (newNodes eq null) {
                  newNodes = mutable.Queue.empty
                }
                newNodes += newSubNode
              }
            } else if (newSubNode.size == 1) {
              newDataMap |= bitpos
              nodeMigrateToDataTargetMap |= bitpos
              if (nodesToMigrateToData eq null) {
                nodesToMigrateToData = mutable.Queue.empty
              }
              nodesToMigrateToData += newSubNode
            }

            nodeIndex += 1
          }

          i += 1
        }
        this.newNodeFrom(
          newSize = newSize,
          newDataMap = newDataMap,
          newNodeMap = newNodeMap,
          minimumIndex = minimumIndex,
          oldDataPassThrough = oldDataPassThrough,
          nodesToPassThroughMap = nodesToPassThroughMap,
          nodeMigrateToDataTargetMap = nodeMigrateToDataTargetMap,
          nodesToMigrateToData = nodesToMigrateToData,
          mapOfNewNodes = mapOfNewNodes,
          newNodes = newNodes,
          newCachedHashCode = newCachedHashCode
        )
      }
    case hashCollisionSetNode: HashCollisionSetNode[A] =>
      // this branch should never happen, because HashCollisionSetNodes and BitMapIndexedSetNodes do not occur at the
      // same depth
      var curr: SetNode[A] = this
      val iter = hashCollisionSetNode.content.iterator
      while(curr.size > 0 && iter.hasNext) {
        val next = iter.next()
        curr = curr.removed(next, hashCollisionSetNode.originalHash, hashCollisionSetNode.hash, shift)
      }
      curr
  }

  /** Utility method only for use in `diff` and `filterImpl`
    *
    * @param newSize the size of the new SetNode
    * @param newDataMap the dataMap of the new SetNode
    * @param newNodeMap the nodeMap of the new SetNode
    * @param minimumIndex the minimum index (in range of [0, 31]) for which there are sub-nodes or data beneath the new
    *                     SetNode
    * @param oldDataPassThrough bitmap representing all the data that are just passed from `this` to the new
    *                           SetNode
    * @param nodesToPassThroughMap bitmap representing all nodes that are just passed from `this` to the new SetNode
    * @param nodeMigrateToDataTargetMap bitmap representing all positions which will now be data in the new SetNode,
    *                                   but which were nodes in `this`
    * @param nodesToMigrateToData a queue (in order of child position) of single-element nodes, which will be migrated
    *                             to data, in positions in the `nodeMigrateToDataTargetMap`
    * @param mapOfNewNodes bitmap of positions of new nodes to include in the new SetNode
    * @param newNodes  queue in order of child position, of all new nodes to include in the new SetNode
    * @param newCachedHashCode the cached java keyset hashcode of the new SetNode
    */
  private[this] def newNodeFrom(
    newSize: Int,
    newDataMap: Int,
    newNodeMap: Int,
    minimumIndex: Int,
    oldDataPassThrough: Int,
    nodesToPassThroughMap: Int,
    nodeMigrateToDataTargetMap: Int,
    nodesToMigrateToData: mutable.Queue[SetNode[A]],
    mapOfNewNodes: Int,
    newNodes: mutable.Queue[SetNode[A]],
    newCachedHashCode: Int): SetNode[A] = {
    if (newSize == 0) {
      SetNode.empty
    } else if (newSize == size) {
      this
    } else {
      val newDataSize = bitCount(newDataMap)
      val newContentSize = newDataSize + bitCount(newNodeMap)
      val newContent = new Array[Any](newContentSize)
      val newOriginalHashes = new Array[Int](newDataSize)

      val newAllMap = newDataMap | newNodeMap
      val maxIndex = Node.BranchingFactor - Integer.numberOfLeadingZeros(newAllMap)

      // note: We MUST start from the minimum index in the old (`this`) node, otherwise `old{Node,Data}Index` will
      // not be incremented properly. Otherwise we could have started at Integer.numberOfTrailingZeroes(newAllMap)
      var i = minimumIndex

      var oldDataIndex = 0
      var oldNodeIndex = 0

      var newDataIndex = 0
      var newNodeIndex = 0

      while (i < maxIndex) {
        val bitpos = bitposFrom(i)

        if ((bitpos & oldDataPassThrough) != 0) {
          newContent(newDataIndex) = getPayload(oldDataIndex)
          newOriginalHashes(newDataIndex) = getHash(oldDataIndex)
          newDataIndex += 1
          oldDataIndex += 1
        } else if ((bitpos & nodesToPassThroughMap) != 0) {
          newContent(newContentSize - newNodeIndex - 1) = getNode(oldNodeIndex)
          newNodeIndex += 1
          oldNodeIndex += 1
        } else if ((bitpos & nodeMigrateToDataTargetMap) != 0) {
          // we need not check for null here. If nodeMigrateToDataTargetMap != 0, then nodesMigrateToData must not be null
          val node = nodesToMigrateToData.dequeue()
          newContent(newDataIndex) = node.getPayload(0)
          newOriginalHashes(newDataIndex) = node.getHash(0)
          newDataIndex += 1
          oldNodeIndex += 1
        } else if ((bitpos & mapOfNewNodes) != 0) {
          // we need not check for null here. If mapOfNewNodes != 0, then newNodes must not be null
          newContent(newContentSize - newNodeIndex - 1) = newNodes.dequeue()
          newNodeIndex += 1
          oldNodeIndex += 1
        } else if ((bitpos & dataMap) != 0) {
          oldDataIndex += 1
        } else if ((bitpos & nodeMap) != 0) {
          oldNodeIndex += 1
        }

        i += 1
      }

      new BitmapIndexedSetNode[A](newDataMap, newNodeMap, newContent, newOriginalHashes, newSize, newCachedHashCode)
    }
  }


  override def equals(that: Any): Boolean =
    that match {
      case node: BitmapIndexedSetNode[A] =>
        (this eq node) ||
        (this.cachedJavaKeySetHashCode == node.cachedJavaKeySetHashCode) &&
          (this.nodeMap == node.nodeMap) &&
            (this.dataMap == node.dataMap) &&
              (this.size == node.size) &&
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

  override def copy(): BitmapIndexedSetNode[A] = {
    val contentClone = content.clone()
    val contentLength = contentClone.length
    var i = bitCount(dataMap)
    while (i < contentLength) {
      contentClone(i) = contentClone(i).asInstanceOf[SetNode[A]].copy()
      i += 1
    }
    new BitmapIndexedSetNode[A](dataMap, nodeMap, contentClone, originalHashes.clone(), size, cachedJavaKeySetHashCode)
  }

  override def concat(that: SetNode[A], shift: Int): SetNode[A] = that match {
    case bm: BitmapIndexedSetNode[A] =>
      if (size == 0) return that
      else if (bm.size == 0 || (bm eq this)) return this
      else if (bm.size == 1) {
        val originalHash = bm.getHash(0)
        return this.updated(bm.getPayload(0), originalHash, improve(originalHash), shift)
      }

      // if we go through the merge and the result does not differ from `this`, we can just return `this`, to improve sharing
      // So, `anyChangesMadeSoFar` will be set to `true` as soon as we encounter a difference between the
      // currently-being-computed result, and `this`
      var anyChangesMadeSoFar = false

      // bitmap containing `1` in any position that has any descendant in either left or right, either data or node
      val allMap = dataMap | bm.dataMap | nodeMap | bm.nodeMap

      // minimumIndex is inclusive -- it is the first index for which there is data or nodes
      val minimumBitPos: Int = Node.bitposFrom(Integer.numberOfTrailingZeros(allMap))
      // maximumIndex is inclusive -- it is the last index for which there is data or nodes
      // it could not be exclusive, because then upper bound in worst case (Node.BranchingFactor) would be out-of-bound
      // of int bitposition representation
      val maximumBitPos: Int = Node.bitposFrom(Node.BranchingFactor - Integer.numberOfLeadingZeros(allMap) - 1)

      var leftNodeRightNode = 0
      var leftDataRightNode = 0
      var leftNodeRightData = 0
      var leftDataOnly = 0
      var rightDataOnly = 0
      var leftNodeOnly = 0
      var rightNodeOnly = 0
      var leftDataRightDataMigrateToNode = 0
      var leftDataRightDataLeftOverwrites = 0

      var dataToNodeMigrationTargets = 0

      {
        var bitpos = minimumBitPos
        var leftIdx = 0
        var rightIdx = 0
        var finished = false

        while (!finished) {

          if ((bitpos & dataMap) != 0) {
            if ((bitpos & bm.dataMap) != 0) {
              if (getHash(leftIdx) == bm.getHash(rightIdx) && getPayload(leftIdx) == bm.getPayload(rightIdx)) {
                leftDataRightDataLeftOverwrites |= bitpos
              } else {
                leftDataRightDataMigrateToNode |= bitpos
                dataToNodeMigrationTargets |= Node.bitposFrom(Node.maskFrom(improve(getHash(leftIdx)), shift))
              }
              rightIdx += 1
            } else if ((bitpos & bm.nodeMap) != 0) {
              leftDataRightNode |= bitpos
            } else {
              leftDataOnly |= bitpos
            }
            leftIdx += 1
          } else if ((bitpos & nodeMap) != 0) {
            if ((bitpos & bm.dataMap) != 0) {
              leftNodeRightData |= bitpos
              rightIdx += 1
            } else if ((bitpos & bm.nodeMap) != 0) {
              leftNodeRightNode |= bitpos
            } else {
              leftNodeOnly |= bitpos
            }
          } else if ((bitpos & bm.dataMap) != 0) {
            rightDataOnly |= bitpos
            rightIdx += 1
          } else if ((bitpos & bm.nodeMap) != 0) {
            rightNodeOnly |= bitpos
          }

          if (bitpos == maximumBitPos) {
            finished = true
          } else {
            bitpos = bitpos << 1
          }
        }
      }


      val newDataMap = leftDataOnly | rightDataOnly | leftDataRightDataLeftOverwrites

      val newNodeMap =
        leftNodeRightNode |
          leftDataRightNode |
          leftNodeRightData |
          leftNodeOnly |
          rightNodeOnly |
          dataToNodeMigrationTargets


      if ((newDataMap == (leftDataOnly | leftDataRightDataLeftOverwrites)) && (newNodeMap == leftNodeOnly)) {
        // nothing from `bm` will make it into the result -- return early
        return this
      }

      val newDataSize = bitCount(newDataMap)
      val newContentSize = newDataSize + bitCount(newNodeMap)

      val newContent = new Array[Any](newContentSize)
      val newOriginalHashes = new Array[Int](newDataSize)
      var newSize = 0
      var newCachedHashCode = 0

      {
        var leftDataIdx = 0
        var rightDataIdx = 0
        var leftNodeIdx = 0
        var rightNodeIdx = 0

        val nextShift = shift + Node.BitPartitionSize

        var compressedDataIdx = 0
        var compressedNodeIdx = 0

        var bitpos = minimumBitPos
        var finished = false

        while (!finished) {

          if ((bitpos & leftNodeRightNode) != 0) {
            val leftNode = getNode(leftNodeIdx)
            val newNode = leftNode.concat(bm.getNode(rightNodeIdx), nextShift)
            if (leftNode ne newNode) {
              anyChangesMadeSoFar = true
            }
            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            rightNodeIdx += 1
            leftNodeIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode

          } else if ((bitpos & leftDataRightNode) != 0) {
            anyChangesMadeSoFar = true
            val newNode = {
              val n = bm.getNode(rightNodeIdx)
              val leftPayload = getPayload(leftDataIdx)
              val leftOriginalHash = getHash(leftDataIdx)
              val leftImproved = improve(leftOriginalHash)
              n.updated(leftPayload, leftOriginalHash, leftImproved, nextShift)
            }

            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            rightNodeIdx += 1
            leftDataIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          }
          else if ((bitpos & leftNodeRightData) != 0) {
            val newNode = {
              val rightOriginalHash = bm.getHash(rightDataIdx)
              val leftNode = getNode(leftNodeIdx)
              val updated = leftNode.updated(
                element = bm.getPayload(rightDataIdx),
                originalHash = bm.getHash(rightDataIdx),
                hash = improve(rightOriginalHash),
                shift = nextShift
              )
              if (updated ne leftNode) {
                anyChangesMadeSoFar = true
              }
              updated
            }

            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            leftNodeIdx += 1
            rightDataIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode

          } else if ((bitpos & leftDataOnly) != 0) {
            val originalHash = originalHashes(leftDataIdx)
            newContent(compressedDataIdx) = getPayload(leftDataIdx).asInstanceOf[AnyRef]
            newOriginalHashes(compressedDataIdx) = originalHash

            compressedDataIdx += 1
            leftDataIdx += 1
            newSize += 1
            newCachedHashCode += improve(originalHash)
          } else if ((bitpos & rightDataOnly) != 0) {
            anyChangesMadeSoFar = true
            val originalHash = bm.originalHashes(rightDataIdx)
            newContent(compressedDataIdx) = bm.getPayload(rightDataIdx).asInstanceOf[AnyRef]
            newOriginalHashes(compressedDataIdx) = originalHash

            compressedDataIdx += 1
            rightDataIdx += 1
            newSize += 1
            newCachedHashCode += improve(originalHash)
          } else if ((bitpos & leftNodeOnly) != 0) {
            val newNode = getNode(leftNodeIdx)
            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            leftNodeIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          } else if ((bitpos & rightNodeOnly) != 0) {
            anyChangesMadeSoFar = true
            val newNode = bm.getNode(rightNodeIdx)
            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            rightNodeIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          } else if ((bitpos & leftDataRightDataMigrateToNode) != 0) {
            anyChangesMadeSoFar = true
            val newNode = {
              val leftOriginalHash = getHash(leftDataIdx)
              val rightOriginalHash = bm.getHash(rightDataIdx)

              bm.mergeTwoKeyValPairs(
                getPayload(leftDataIdx), leftOriginalHash, improve(leftOriginalHash),
                bm.getPayload(rightDataIdx), rightOriginalHash, improve(rightOriginalHash),
                nextShift
              )
            }

            newContent(newContentSize - compressedNodeIdx - 1) = newNode
            compressedNodeIdx += 1
            leftDataIdx += 1
            rightDataIdx += 1
            newSize += newNode.size
            newCachedHashCode += newNode.cachedJavaKeySetHashCode
          } else if ((bitpos & leftDataRightDataLeftOverwrites) != 0) {
            val originalHash = bm.originalHashes(rightDataIdx)
            newContent(compressedDataIdx) = bm.getPayload(rightDataIdx).asInstanceOf[AnyRef]
            newOriginalHashes(compressedDataIdx) = originalHash

            compressedDataIdx += 1
            rightDataIdx += 1
            newSize += 1
            newCachedHashCode += improve(originalHash)
            leftDataIdx += 1
          }

          if (bitpos == maximumBitPos) {
            finished = true
          } else {
            bitpos = bitpos << 1
          }
        }
      }

      if (anyChangesMadeSoFar)
        new BitmapIndexedSetNode(
          dataMap = newDataMap,
          nodeMap = newNodeMap,
          content = newContent,
          originalHashes = newOriginalHashes,
          size = newSize,
          cachedJavaKeySetHashCode = newCachedHashCode
        )
      else this

    case _ =>
      // should never happen -- hash collisions are never at the same level as bitmapIndexedSetNodes
      throw new UnsupportedOperationException("Cannot concatenate a HashCollisionSetNode with a BitmapIndexedSetNode")
  }

  override def foreachWithHash(f: (A, Int) => Unit): Unit = {
    val iN = payloadArity // arity doesn't change during this operation
    var i = 0
    while (i < iN) {
      f(getPayload(i), getHash(i))
      i += 1
    }

    val jN = nodeArity // arity doesn't change during this operation
    var j = 0
    while (j < jN) {
      getNode(j).foreachWithHash(f)
      j += 1
    }
  }

  override def foreachWithHashWhile(f: (A, Int) => Boolean): Boolean = {
    val thisPayloadArity = payloadArity
    var pass = true
    var i = 0
    while (i < thisPayloadArity && pass) {
      pass &&= f(getPayload(i), getHash(i))
      i += 1
    }

    val thisNodeArity = nodeArity
    var j = 0
    while (j < thisNodeArity && pass) {
      pass &&= getNode(j).foreachWithHashWhile(f)
      j += 1
    }
    pass
  }
}

private final class HashCollisionSetNode[A](val originalHash: Int, val hash: Int, var content: Vector[A]) extends SetNode[A] {

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
        case 1 => new BitmapIndexedSetNode[A](bitposFrom(maskFrom(hash, 0)), 0, Array(updatedContent(0)), Array(originalHash), 1, hash)
        case _ => new HashCollisionSetNode[A](originalHash, hash, updatedContent)
      }
    }

  def hasNodes: Boolean = false

  def nodeArity: Int = 0

  def getNode(index: Int): SetNode[A] =
    throw new IndexOutOfBoundsException("No sub-nodes present in hash-collision leaf node.")

  def hasPayload: Boolean = true

  def payloadArity: Int = content.length

  def getPayload(index: Int): A = content(index)

  override def getHash(index: Int): Int = originalHash

  def size: Int = content.length

  def foreach[U](f: A => U): Unit = {
    val iter = content.iterator
    while (iter.hasNext) {
      f(iter.next())
    }
  }


  override def cachedJavaKeySetHashCode: Int = size * hash

  def subsetOf(that: SetNode[A], shift: Int): Boolean = if (this eq that) true else that match {
    case node: HashCollisionSetNode[A] =>
      this.payloadArity <= node.payloadArity && this.content.forall(node.content.contains)
    case _ =>
      false
  }

  override def filterImpl(pred: A => Boolean, flipped: Boolean): SetNode[A] = {
    val newContent = content.filterImpl(pred, flipped)
    val newContentLength = newContent.length
    if (newContentLength == 0) {
      SetNode.empty
    } else if (newContentLength == 1) {
      new BitmapIndexedSetNode[A](bitposFrom(maskFrom(hash, 0)), 0, Array(newContent.head), Array(originalHash), 1, hash)
    } else if (newContent.length == content.length) this
    else new HashCollisionSetNode(originalHash, hash, newContent)
  }

  override def diff(that: SetNode[A], shift: Int): SetNode[A] =
    filterImpl(that.contains(_, originalHash, hash, shift), true)

  override def equals(that: Any): Boolean =
    that match {
      case node: HashCollisionSetNode[A] =>
        (this eq node) ||
          (this.hash == node.hash) &&
            (this.content.size == node.content.size) &&
            this.content.forall(node.content.contains)
      case _ => false
    }

  override def hashCode(): Int =
    throw new UnsupportedOperationException("Trie nodes do not support hashing.")

  override def copy() = new HashCollisionSetNode[A](originalHash, hash, content)

  override def concat(that: SetNode[A], shift: Int): SetNode[A] = that match {
    case hc: HashCollisionSetNode[A] =>
      if (hc eq this) {
        this
      } else {
        var newContent: VectorBuilder[A] = null
        val iter = hc.content.iterator
        while (iter.hasNext) {
          val nextPayload = iter.next()
          if (!content.contains(nextPayload)) {
            if (newContent eq null) {
              newContent = new VectorBuilder()
              newContent.addAll(this.content)
            }
            newContent.addOne(nextPayload)
          }
        }
        if (newContent eq null) this else new HashCollisionSetNode(originalHash, hash, newContent.result())
      }
    case _: BitmapIndexedSetNode[A] =>
      // should never happen -- hash collisions are never at the same level as bitmapIndexedSetNodes
      throw new UnsupportedOperationException("Cannot concatenate a HashCollisionSetNode with a BitmapIndexedSetNode")
  }

  override def foreachWithHash(f: (A, Int) => Unit): Unit = {
    val iter = content.iterator
    while (iter.hasNext) {
      val next = iter.next()
      f(next.asInstanceOf[A], originalHash)
    }
  }

  override def foreachWithHashWhile(f: (A, Int) => Boolean): Boolean = {
    var stillGoing = true
    val iter = content.iterator
    while (iter.hasNext && stillGoing) {
      val next = iter.next()
      stillGoing &&= f(next.asInstanceOf[A], originalHash)
    }
    stillGoing
  }
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

  def next(): A = {
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
    new HashSet[A](rootNode)

  @transient
  private final val EmptySet = new HashSet(SetNode.empty)

  def empty[A]: HashSet[A] =
    EmptySet.asInstanceOf[HashSet[A]]

  def from[A](source: collection.IterableOnce[A]): HashSet[A] =
    source match {
      case hs: HashSet[A] => hs
      case _ if source.knownSize == 0 => empty[A]
      case _ => (newBuilder[A] ++= source).result()
    }

  /** Create a new Builder which can be reused after calling `result()` without an
    * intermediate call to `clear()` in order to build multiple related results.
    */
  def newBuilder[A]: ReusableBuilder[A, HashSet[A]] = new HashSetBuilder
}

/** Builder for HashSet.
  * $multipleResults
  */
private[collection] final class HashSetBuilder[A] extends ReusableBuilder[A, HashSet[A]] {
  import Node._
  import SetNode._

  private def newEmptyRootNode = new BitmapIndexedSetNode[A](0, 0, Array(), Array(), 0, 0)

  /** The last given out HashSet as a return value of `result()`, if any, otherwise null.
    * Indicates that on next add, the elements should be copied to an identical structure, before continuing
    * mutations. */
  private var aliased: HashSet[A] = _

  private def isAliased: Boolean = aliased != null

  /** The root node of the partially build hashmap */
  private var rootNode: SetNode[A] = newEmptyRootNode

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
  private def insertValue[A1 >: A](bm: BitmapIndexedSetNode[A], bitpos: Int, key: A, originalHash: Int, keyHash: Int): Unit = {
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
    bm.cachedJavaKeySetHashCode += keyHash
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
  private def migrateFromInlineToNode(bm: BitmapIndexedSetNode[A], elementHash: Int, bitpos: Int, node: SetNode[A]): Unit = {
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

    bm.dataMap ^= bitpos
    bm.nodeMap |= bitpos
    bm.content = dst
    bm.originalHashes = dstHashes
    bm.size = bm.size - 1 + node.size
    bm.cachedJavaKeySetHashCode = bm.cachedJavaKeySetHashCode - elementHash + node.cachedJavaKeySetHashCode
  }

  /** Mutates `bm` to replace inline data at bit position `bitpos` with updated key/value */
  private def setValue[A1 >: A](bm: BitmapIndexedSetNode[A], bitpos: Int, elem: A): Unit = {
    val dataIx = bm.dataIndex(bitpos)
    val idx = TupleLength * dataIx
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
          val element0UnimprovedHash = bm.getHash(index)

          if (element0UnimprovedHash == originalHash && element0 == element) {
            setValue(bm, bitpos, element0)
          } else {
            val element0Hash = improve(element0UnimprovedHash)
            val subNodeNew = bm.mergeTwoKeyValPairs(element0, element0UnimprovedHash, element0Hash, element, originalHash, elementHash, shift + BitPartitionSize)
            migrateFromInlineToNode(bm, element0Hash, bitpos, subNodeNew)
          }
        } else if ((bm.nodeMap & bitpos) != 0) {
          val index = indexFrom(bm.nodeMap, mask, bitpos)
          val subNode = bm.getNode(index)
          val beforeSize = subNode.size
          val beforeHashCode = subNode.cachedJavaKeySetHashCode
          update(subNode, element, originalHash, elementHash, shift + BitPartitionSize)
          bm.size += subNode.size - beforeSize
          bm.cachedJavaKeySetHashCode += subNode.cachedJavaKeySetHashCode - beforeHashCode
        } else {
          insertValue(bm, bitpos, element, originalHash, elementHash)
        }
      case hc: HashCollisionSetNode[A] =>
        val index = hc.content.indexOf(element)
        if (index < 0) {
          hc.content = hc.content.appended(element)
        } else {
          hc.content = hc.content.updated(index, element)
        }
    }

  /** If currently referencing aliased structure, copy elements to new mutable structure */
  private def ensureUnaliased():Unit = {
    if (isAliased) copyElems()
    aliased = null
  }

  /** Copy elements to new mutable structure */
  private def copyElems(): Unit = {
    rootNode = rootNode.copy()
  }

  override def result(): HashSet[A] =
    if (rootNode.size == 0) {
      HashSet.empty
    } else if (aliased != null) {
      aliased
    } else {
      aliased = new HashSet(rootNode)
      releaseFence()
      aliased
    }

  override def addOne(elem: A): this.type = {
    ensureUnaliased()
    val h = elem.##
    val im = improve(h)
    update(rootNode, elem, h, im, 0)
    this
  }

  override def addAll(xs: IterableOnce[A]) = {
    ensureUnaliased()
    xs match {
      case hm: HashSet[A] =>
        new ChampBaseIterator(hm.rootNode) {
          while(hasNext) {
            val originalHash = currentValueNode.getHash(currentValueCursor)
            update(
              setNode = rootNode,
              element = currentValueNode.getPayload(currentValueCursor),
              originalHash = originalHash,
              elementHash = improve(originalHash),
              shift = 0
            )
            currentValueCursor += 1
          }
        }
      case other =>
        val it = other.iterator
        while(it.hasNext) addOne(it.next())
    }

    this
  }

  override def clear(): Unit = {
    aliased = null
    if (rootNode.size > 0) {
      // if rootNode is empty, we will not have given it away anyways, we instead give out the reused Set.empty
      rootNode = newEmptyRootNode
    }
  }

  private[collection] def size: Int = rootNode.size

  override def knownSize: Int = rootNode.size
}
