package scala
package collection.immutable

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.lang.Integer.bitCount
import java.lang.System.arraycopy

import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.collection.Hashing.improve
import scala.collection.mutable.Builder
import scala.collection.{Iterator, MapFactory, StrictOptimizedIterableOps, StrictOptimizedMapOps}
import scala.util.hashing.MurmurHash3
import scala.runtime.Statics.releaseFence

/** This class implements immutable maps using a Compressed Hash-Array Mapped Prefix-tree.
  * See paper https://michael.steindorfer.name/publications/oopsla15.pdf for more details.
  *
  *  @tparam K      the type of the keys contained in this hash set.
  *  @tparam V      the type of the values associated with the keys in this hash map.
  *
  *  @author  Michael J. Steindorfer
  *  @since   2.13
  *  @define Coll `immutable.HashMap`
  *  @define coll immutable champ hash map
  */

final class HashMap[K, +V] private[immutable] (private[immutable] val rootNode: MapNode[K, V], private val cachedJavaKeySetHashCode: Int)
  extends AbstractMap[K, V]
    with MapOps[K, V, HashMap, HashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, HashMap[K, V]]
    with StrictOptimizedMapOps[K, V, HashMap, HashMap[K, V]] {

  def this() = this(MapNode.empty, 0)

  releaseFence()

  override def mapFactory: MapFactory[HashMap] = HashMap

  override def knownSize: Int = rootNode.size

  override def size: Int = rootNode.size

  override def isEmpty: Boolean = rootNode.size == 0

  def iterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else new MapKeyValueTupleIterator[K, V](rootNode)
  }

  override def keysIterator: Iterator[K] = {
    if (isEmpty) Iterator.empty
    else new MapKeyIterator[K, V](rootNode)
  }
  override def valuesIterator: Iterator[V] = {
    if (isEmpty) Iterator.empty
    else new MapValueIterator[K, V](rootNode)
  }

  protected[immutable] def reverseIterator: Iterator[(K, V)] = {
    if (isEmpty) Iterator.empty
    else new MapKeyValueTupleReverseIterator[K, V](rootNode)
  }

  override final def contains(key: K): Boolean = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)
    rootNode.containsKey(key, keyUnimprovedHash, keyHash, 0)
  }

  def get(key: K): Option[V] = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)
    rootNode.get(key, keyUnimprovedHash, keyHash, 0)
  }

  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)
    rootNode.getOrElse(key, keyUnimprovedHash, keyHash, 0, default)
  }

  def updated[V1 >: V](key: K, value: V1): HashMap[K, V1] = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)

    val newRootNode = rootNode.updated(key, value, keyUnimprovedHash, keyHash, 0)

    if (newRootNode ne rootNode) {
      val replaced = rootNode.size == newRootNode.size
      val newCachedJavaKeySetHashCode = cachedJavaKeySetHashCode + (if (replaced) 0 else keyHash)
      HashMap(newRootNode.get, newCachedJavaKeySetHashCode)
    } else
      this
  }

  def remove(key: K): HashMap[K, V] = {
    val keyUnimprovedHash = key.##
    val keyHash = improve(keyUnimprovedHash)

    val newRootNode = rootNode.removed(key, keyUnimprovedHash, keyHash, 0)

    if (newRootNode ne rootNode)
      HashMap(newRootNode, cachedJavaKeySetHashCode - keyHash)
    else this
  }

  override def concat[V1 >: V](that: scala.IterableOnce[(K, V1)]): HashMap[K, V1] = {
    // TODO PERF We could avoid recomputing entry hash's when `that` is another `HashMap`
    val builder = mapFactory.newBuilder[K, V1]
    builder ++= this
    builder ++= that
    builder.result()
  }

  override def tail: HashMap[K, V] = this - head._1

  override def init: HashMap[K, V] = this - last._1

  override def head: (K, V) = iterator.next()

  override def last: (K, V) = reverseIterator.next()

  override def foreach[U](f: ((K, V)) => U): Unit = rootNode.foreach(f)

  override def equals(that: Any): Boolean =
    that match {
      case map: HashMap[K, V] =>
        (this eq map) ||
          (this.size == map.size) &&
            (this.cachedJavaKeySetHashCode == map.cachedJavaKeySetHashCode) &&
            (this.rootNode == map.rootNode)
      case _ => super.equals(that)
    }

  override def hashCode(): Int = {
    val hashIterator = new MapKeyValueTupleHashIterator(rootNode)
    val hash = MurmurHash3.unorderedHash(hashIterator, MurmurHash3.mapSeed)
    // assert(hash == super.hashCode())
    hash
  }

  override protected[this] def className = "HashMap"

  private type MergeFunction[A1, B1] = ((A1, B1), (A1, B1)) => (A1, B1)

  //TODO optimize (https://github.com/scala/bug/issues/11077)
  def merged[V1 >: V](that: HashMap[K, V1])(mergef: MergeFunction[K, V1]): HashMap[K, V1] = {
    val thisKeys = this.keySet
    if(mergef eq null)
      that.removeAll(thisKeys) ++ this
    else {
      val thatKeys = that.keySet
      that.removeAll(thisKeys) ++ this.removeAll(thatKeys) ++ thisKeys.intersect(thatKeys).map { case k => mergef((k, this(k)), (k, that(k))) }
    }
  }
}

private[immutable] object MapNode {

  private final val EmptyMapNode = new BitmapIndexedMapNode(0, 0, Array.empty, Array.empty, 0)

  def empty[K, V]: MapNode[K, V] =
    EmptyMapNode.asInstanceOf[MapNode[K, V]]

  final val TupleLength = 2

}


private[immutable] sealed abstract class MapNode[K, +V] extends Node[MapNode[K, V @uV]] {
  final def get: MapNode[K, V] = this

  def get(key: K, originalHash: Int, hash: Int, shift: Int): Option[V]
  def getOrElse[V1 >: V](key: K, originalHash: Int, hash: Int, shift: Int, f: => V1): V1

  def containsKey(key: K, originalHash: Int, hash: Int, shift: Int): Boolean

  def updated[V1 >: V](key: K, value: V1, originalHash: Int, hash: Int, shift: Int): MapNode[K, V1]

  def removed[V1 >: V](key: K, hash: Int, originalHash: Int, shift: Int): MapNode[K, V1]

  def hasNodes: Boolean

  def nodeArity: Int

  def getNode(index: Int): MapNode[K, V]

  def hasPayload: Boolean

  def payloadArity: Int

  def getKey(index: Int): K

  def getValue(index: Int): V

  def getPayload(index: Int): (K, V)

  def sizePredicate: Int

  def size: Int

  def foreach[U](f: ((K, V)) => U): Unit
}

private final class BitmapIndexedMapNode[K, +V](
  var dataMap: Int,
  var nodeMap: Int,
  var content: Array[Any],
  var originalHashes: Array[Int],
  var size: Int) extends MapNode[K, V] {

  releaseFence()

  import MapNode._
  import Node._

  /*
  assert(checkInvariantContentIsWellTyped())
  assert(checkInvariantSubNodesAreCompacted())

  private final def checkInvariantSubNodesAreCompacted(): Boolean =
    new MapKeyValueTupleIterator[K, V](this).size - payloadArity >= 2 * nodeArity

  private final def checkInvariantContentIsWellTyped(): Boolean = {
    val predicate1 = TupleLength * payloadArity + nodeArity == content.length

    val predicate2 = Range(0, TupleLength * payloadArity)
      .forall(i => content(i).isInstanceOf[MapNode[_, _]] == false)

    val predicate3 = Range(TupleLength * payloadArity, content.length)
      .forall(i => content(i).isInstanceOf[MapNode[_, _]] == true)

    predicate1 && predicate2 && predicate3
  }
  */

  def getKey(index: Int): K = content(TupleLength * index).asInstanceOf[K]
  def getValue(index: Int): V = content(TupleLength * index + 1).asInstanceOf[V]

  def getPayload(index: Int) = Tuple2(
    content(TupleLength * index).asInstanceOf[K],
    content(TupleLength * index + 1).asInstanceOf[V])

  override def getHash(index: Int): Int = originalHashes(index)

  def getNode(index: Int) =
    content(content.length - 1 - index).asInstanceOf[MapNode[K, V]]

  def get(key: K, originalHash: Int, keyHash: Int, shift: Int): Option[V] = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = this.getKey(index)
      return if (key == key0) Some(this.getValue(index)) else None
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      return this.getNode(index).get(key, originalHash, keyHash, shift + BitPartitionSize)
    }

    None
  }
  def getOrElse[V1 >: V](key: K, originalHash: Int, keyHash: Int, shift: Int, f: => V1): V1 = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = this.getKey(index)
      return if (key == key0) this.getValue(index) else f
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      return this.getNode(index).getOrElse(key, originalHash, keyHash, shift + BitPartitionSize, f)
    }
    f
  }

  override def containsKey(key: K, originalHash: Int, keyHash: Int, shift: Int): Boolean = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      // assert(hashes(index) == computeHash(this.getKey(index)), (hashes.toSeq, content.toSeq, index, key, keyHash, shift))
      return (originalHashes(index) == originalHash) && key == this.getKey(index)
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      return this.getNode(index).containsKey(key, originalHash, keyHash, shift + BitPartitionSize)
    }

    false
  }

  def updated[V1 >: V](key: K, value: V1, originalHash: Int, keyHash: Int, shift: Int): MapNode[K, V1] = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = this.getKey(index)
      if (key0 == key) {
        val value0 = this.getValue(index)
        return (
          if ((key0.asInstanceOf[AnyRef] eq key.asInstanceOf[AnyRef]) && (value0.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]))
            this
          else copyAndSetValue(bitpos, key, value)
        )
      } else {
        val value0 = this.getValue(index)
        val key0UnimprovedHash = key0.##
        val key0Hash = improve(key0UnimprovedHash)

        val subNodeNew = mergeTwoKeyValPairs(key0, value0, key0UnimprovedHash, key0Hash, key, value, originalHash, keyHash, shift + BitPartitionSize)
        return copyAndMigrateFromInlineToNode(bitpos, subNodeNew)
      }
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.updated(key, value, originalHash, keyHash, shift + BitPartitionSize)
      if (subNodeNew eq subNode) {
        return this
      } else {
        return copyAndSetNode(bitpos, subNode, subNodeNew)
      }
    }

    copyAndInsertValue(bitpos, key, originalHash, keyHash, value)
  }

  def removed[V1 >: V](key: K, originalHash: Int, keyHash: Int, shift: Int): MapNode[K, V1] = {
    val mask = maskFrom(keyHash, shift)
    val bitpos = bitposFrom(mask)

    if ((dataMap & bitpos) != 0) {
      val index = indexFrom(dataMap, mask, bitpos)
      val key0 = this.getKey(index)

      if (key0 == key) {
        if (this.payloadArity == 2 && this.nodeArity == 0) {
          /*
           * Create new node with remaining pair. The new node will a) either become the new root
           * returned, or b) unwrapped and inlined during returning.
           */
          val newDataMap = if (shift == 0) (dataMap ^ bitpos) else bitposFrom(maskFrom(keyHash, 0))
          if (index == 0)
            return new BitmapIndexedMapNode[K, V1](newDataMap, 0, Array(getKey(1), getValue(1)), Array(originalHashes(1)), 1)
          else
            return new BitmapIndexedMapNode[K, V1](newDataMap, 0, Array(getKey(0), getValue(0)), Array(originalHashes(0)), 1)
        }
        else return copyAndRemoveValue(bitpos)
      } else return this
    }

    if ((nodeMap & bitpos) != 0) {
      val index = indexFrom(nodeMap, mask, bitpos)
      val subNode = this.getNode(index)

      val subNodeNew = subNode.removed(key, originalHash, keyHash, shift + BitPartitionSize)
      // assert(subNodeNew.sizePredicate != SizeEmpty, "Sub-node must have at least one element.")

      if (subNodeNew eq subNode) return this
      subNodeNew.sizePredicate match {
        case SizeOne =>
          if (this.payloadArity == 0 && this.nodeArity == 1) { // escalate (singleton or empty) result
            return subNodeNew
          }
          else { // inline value (move to front)
            return copyAndMigrateFromNodeToInline(bitpos, originalHash, keyHash, subNode, subNodeNew)
          }

        case SizeMoreThanOne =>
          // modify current node (set replacement node)
          return copyAndSetNode(bitpos, subNode, subNodeNew)
      }
    }

    this
  }

  def mergeTwoKeyValPairs[V1 >: V](key0: K, value0: V1, originalHash0: Int, keyHash0: Int, key1: K, value1: V1, originalHash1: Int, keyHash1: Int, shift: Int): MapNode[K, V1] = {
    // assert(key0 != key1)

    if (shift >= HashCodeLength) {
      new HashCollisionMapNode[K, V1](originalHash0, keyHash0, Array(key0, key1), Array(value0, value1))
    } else {
      val mask0 = maskFrom(keyHash0, shift)
      val mask1 = maskFrom(keyHash1, shift)

      if (mask0 != mask1) {
        // unique prefixes, payload fits on same level
        val dataMap = bitposFrom(mask0) | bitposFrom(mask1)

        if (mask0 < mask1) {
          new BitmapIndexedMapNode[K, V1](dataMap, 0, Array(key0, value0, key1, value1), Array(originalHash0, originalHash1), 2)
        } else {
          new BitmapIndexedMapNode[K, V1](dataMap, 0, Array(key1, value1, key0, value0), Array(originalHash1, originalHash0), 2)
        }
      } else {
        // identical prefixes, payload must be disambiguated deeper in the trie
        val nodeMap = bitposFrom(mask0)
        val node = mergeTwoKeyValPairs(key0, value0, originalHash0, keyHash0, key1, value1, originalHash1, keyHash1, shift + BitPartitionSize)
        new BitmapIndexedMapNode[K, V1](0, nodeMap, Array(node), Array(), node.size)
      }
    }
  }
  
  def sizePredicate: Int =
    if (nodeArity == 0) payloadArity match {
      case 0 => SizeEmpty
      case 1 => SizeOne
      case _ => SizeMoreThanOne
    } else SizeMoreThanOne

  def hasNodes: Boolean = nodeMap != 0

  def nodeArity: Int = bitCount(nodeMap)

  def hasPayload: Boolean = dataMap != 0

  def payloadArity: Int = bitCount(dataMap)

  def dataIndex(bitpos: Int) = bitCount(dataMap & (bitpos - 1))

  def nodeIndex(bitpos: Int) = bitCount(nodeMap & (bitpos - 1))

  def copyAndSetValue[V1 >: V](bitpos: Int, newKey: K, newValue: V1): MapNode[K, V1] = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    dst(idx) = newKey
    dst(idx + 1) = newValue
    new BitmapIndexedMapNode[K, V1](dataMap, nodeMap, dst, originalHashes, size)
  }

  def copyAndSetNode[V1 >: V](bitpos: Int, oldNode: MapNode[K, V1], newNode: MapNode[K, V1]): MapNode[K, V1] = {
    val idx = this.content.length - 1 - this.nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length)

    // copy 'src' and set 1 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, src.length)
    dst(idx) = newNode
    new BitmapIndexedMapNode[K, V1](dataMap, nodeMap, dst, originalHashes, size - oldNode.size + newNode.size)
  }

  def copyAndInsertValue[V1 >: V](bitpos: Int, key: K, originalHash: Int, keyHash: Int, value: V1): BitmapIndexedMapNode[K, V1] = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length + TupleLength)

    // copy 'src' and insert 2 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    dst(idx) = key
    dst(idx + 1) = value
    arraycopy(src, idx, dst, idx + TupleLength, src.length - idx)

    val dstHashes = insertElement(originalHashes, dataIx, originalHash)

    new BitmapIndexedMapNode[K, V1](dataMap | bitpos, nodeMap, dst, dstHashes, size + 1)
  }

  def copyAndRemoveValue(bitpos: Int): BitmapIndexedMapNode[K, V] = {
    val dataIx = dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = this.content
    val dst = new Array[Any](src.length - TupleLength)

    // copy 'src' and remove 2 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    arraycopy(src, idx + TupleLength, dst, idx, src.length - idx - TupleLength)

    val dstHashes = removeElement(originalHashes, dataIx)

    new BitmapIndexedMapNode[K, V](dataMap ^ bitpos, nodeMap, dst, dstHashes, size - 1)
  }

  def copyAndMigrateFromInlineToNode[V1 >: V](bitpos: Int, node: MapNode[K, V1]): BitmapIndexedMapNode[K, V1] = {
    val dataIx = dataIndex(bitpos)
    val idxOld = TupleLength * dataIx
    val idxNew = this.content.length - TupleLength - nodeIndex(bitpos)

    val src = this.content
    val dst = new Array[Any](src.length - TupleLength + 1)

    // copy 'src' and remove 2 element(s) at position 'idxOld' and
    // insert 1 element(s) at position 'idxNew'
    // assert(idxOld <= idxNew)
    arraycopy(src, 0, dst, 0, idxOld)
    arraycopy(src, idxOld + TupleLength, dst, idxOld, idxNew - idxOld)
    dst(idxNew) = node
    arraycopy(src, idxNew + TupleLength, dst, idxNew + 1, src.length - idxNew - TupleLength)

    val dstHashes = removeElement(originalHashes, dataIx)

    new BitmapIndexedMapNode[K, V1](dataMap ^ bitpos, nodeMap | bitpos, dst, dstHashes, size - 1 + node.size)
  }

  def copyAndMigrateFromNodeToInline[V1 >: V](bitpos: Int, originalHash: Int, keyHash: Int, oldNode: MapNode[K, V1], node: MapNode[K, V1]): BitmapIndexedMapNode[K, V1] = {
    val idxOld = this.content.length - 1 - nodeIndex(bitpos)
    val dataIxNew = dataIndex(bitpos)
    val idxNew = TupleLength * dataIxNew

    val key = node.getKey(0)
    val value = node.getValue(0)
    val src = this.content
    val dst = new Array[Any](src.length - 1 + TupleLength)

    // copy 'src' and remove 1 element(s) at position 'idxOld' and
    // insert 2 element(s) at position 'idxNew'
    // assert(idxOld >= idxNew)
    arraycopy(src, 0, dst, 0, idxNew)
    dst(idxNew) = key
    dst(idxNew + 1) = value
    arraycopy(src, idxNew, dst, idxNew + TupleLength, idxOld - idxNew)
    arraycopy(src, idxOld + 1, dst, idxOld + TupleLength, src.length - idxOld - 1)
    val hash = node.getHash(0)
    val dstHashes = insertElement(originalHashes, dataIxNew, hash)
    new BitmapIndexedMapNode[K, V1](dataMap | bitpos, nodeMap ^ bitpos, dst, dstHashes, size - oldNode.size + 1)
  }

  override def foreach[U](f: ((K, V)) => U): Unit = {
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

  override def equals(that: Any): Boolean =
    that match {
      case node: BitmapIndexedMapNode[K, V] =>
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

private final class HashCollisionMapNode[K, +V](
  val originalHash: Int,
  val hash: Int,
  var contentKeys: Array[Any],
  var contentValues: Array[Any]) extends MapNode[K, V] {

  import Node._

  require(contentKeys.size >= 2)

  releaseFence()

  def size = contentKeys.size

  def get(key: K, originalHash: Int, hash: Int, shift: Int): Option[V] =
    if (this.hash == hash) {
      val index = contentKeys.indexOf(key)
      if (index >= 0) Some(contentValues(index).asInstanceOf[V]) else None
    } else None

  def getOrElse[V1 >: V](key: K, originalHash: Int, hash: Int, shift: Int, f: => V1): V1 = {
    if (this.hash == hash) {
      contentKeys.indexOf(key) match {
        case -1 => f
        case other => contentValues(other).asInstanceOf[V]
      }
    } else f
  }

  override def containsKey(key: K, originalHash: Int, hash: Int, shift: Int): Boolean =
    this.hash == hash && contentKeys.contains(key)

  def contains[V1 >: V](key: K, value: V1, hash: Int, shift: Int): Boolean =
    this.hash == hash && {
      val index = contentKeys.indexOf(key)
      index >= 0 && (contentValues(index).asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef])
    }

  def updated[V1 >: V](key: K, value: V1, originalHash: Int, hash: Int, shift: Int): MapNode[K, V1] = {
    val index = contentKeys.indexOf(key)
    if (index >= 0) {
      if (contentValues(index).asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]) {
        this
      } else {
        val newContentKeys = contentKeys.clone()
        newContentKeys(index) = key
        val newContentValues = contentValues.clone()
        newContentValues(index) = value
        new HashCollisionMapNode[K, V1](originalHash, hash, newContentKeys, newContentValues)
      }
    } else {
      new HashCollisionMapNode[K, V1](originalHash, hash, contentKeys.appended(key), contentValues.appended(value))
    }
  }

  def removed[V1 >: V](key: K, originalHash: Int, hash: Int, shift: Int): MapNode[K, V1] =
    if (!this.containsKey(key, originalHash, hash, shift)) {
      this
    } else {
      val index = contentKeys.indexOf(key)
      val newContentKeys = removeAnyElement(contentKeys, index)
      val newContentValues = removeAnyElement(contentValues, index)

      // assert(newContentKeys.length == contentKeys.length - 1)

      newContentKeys.length match {
        case 1 =>
          new BitmapIndexedMapNode[K, V1](bitposFrom(maskFrom(hash, 0)), 0, Array(newContentKeys.head, newContentValues.head), Array(originalHash), 1)
        case _ => new HashCollisionMapNode[K, V1](originalHash, hash, newContentKeys, newContentValues)
      }
    }

  def hasNodes: Boolean = false

  def nodeArity: Int = 0

  def getNode(index: Int): MapNode[K, V] =
    throw new IndexOutOfBoundsException("No sub-nodes present in hash-collision leaf node.")

  def hasPayload: Boolean = true

  def payloadArity: Int = contentKeys.length

  def getKey(index: Int): K = getPayload(index)._1
  def getValue(index: Int): V = getPayload(index)._2

  def getPayload(index: Int): (K, V) = (contentKeys(index).asInstanceOf[K], contentValues(index).asInstanceOf[V])
  override def getHash(index: Int): Int = originalHash
  def sizePredicate: Int = SizeMoreThanOne

  def foreach[U](f: ((K, V)) => U): Unit = {
    var i = 0
    while (i < contentKeys.length) {
      f(getPayload(i))
      i += 1
    }
  }

  override def equals(that: Any): Boolean =
    that match {
      case node: HashCollisionMapNode[K, V] =>
        (this eq node) ||
          (this.hash == node.hash) &&
            (this.contentKeys.length == node.contentKeys.length) && {
              var i = 0
              while (i < this.contentKeys.length) {
                val key = contentKeys(i)
                val index = node.contentKeys.indexOf(key)
                if (index < 0 || node.contentValues(index).asInstanceOf[K] != contentValues(index).asInstanceOf[K]) {
                  return false
                }
                i += 1
              }
              true
            }
      case _ => false
    }

  override def hashCode(): Int =
    throw new UnsupportedOperationException("Trie nodes do not support hashing.")

}

private final class MapKeyIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseIterator[MapNode[K, V]](rootNode) with Iterator[K] {

  def next() = {
    if (!hasNext)
      throw new NoSuchElementException

    val key = currentValueNode.getKey(currentValueCursor)
    currentValueCursor += 1

    key
  }

}

private final class MapValueIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseIterator[MapNode[K, V]](rootNode) with Iterator[V] {

  def next() = {
    if (!hasNext)
      throw new NoSuchElementException

    val value = currentValueNode.getValue(currentValueCursor)
    currentValueCursor += 1

    value
  }
}

private final class MapKeyValueTupleIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseIterator[MapNode[K, V]](rootNode) with Iterator[(K, V)] {

  def next() = {
    if (!hasNext)
      throw new NoSuchElementException

    val payload = currentValueNode.getPayload(currentValueCursor)
    currentValueCursor += 1

    payload
  }

}

private final class MapKeyValueTupleReverseIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseReverseIterator[MapNode[K, V]](rootNode) with Iterator[(K, V)] {

  def next() = {
    if (!hasNext)
      throw new NoSuchElementException

    val payload = currentValueNode.getPayload(currentValueCursor)
    currentValueCursor -= 1

    payload
  }

}

private final class MapKeyValueTupleHashIterator[K, V](rootNode: MapNode[K, V])
  extends ChampBaseReverseIterator[MapNode[K, V]](rootNode) with Iterator[Product2[AnyRef, V]] with Product2[AnyRef, V] {
  private[this] var hash = 0
  private[this] var value: V = _
  private[this] val key = new Object {
    override def hashCode(): Int = hash
  }
  override def hashCode(): Int = MurmurHash3.productHash(this)
  override def _1: AnyRef = key
  override def _2: V = value
  override def canEqual(that: Any): Boolean = false
  override def productPrefix: String = "Tuple2"
  def next() = {
    if (!hasNext)
      throw new NoSuchElementException

    hash = currentValueNode.getHash(currentValueCursor)
    value = currentValueNode.getValue(currentValueCursor)
    currentValueCursor -= 1
    this
  }

}

/**
  * $factoryInfo
  *
  * @define Coll `immutable.HashMap`
  * @define coll immutable champ hash map
  */
@SerialVersionUID(3L)
object HashMap extends MapFactory[HashMap] {

  private[HashMap] def apply[K, V](rootNode: MapNode[K, V], cachedJavaHashCode: Int) =
    new HashMap[K, V](rootNode, cachedJavaHashCode)

  private final val EmptyMap = new HashMap(MapNode.empty, 0)

  def empty[K, V]: HashMap[K, V] =
    EmptyMap.asInstanceOf[HashMap[K, V]]

  def from[K, V](source: collection.IterableOnce[(K, V)]): HashMap[K, V] =
    source match {
      case hs: HashMap[K, V] => hs
      case _ => (newBuilder[K, V] ++= source).result()
    }

  def newBuilder[K, V]: Builder[(K, V), HashMap[K, V]] = new HashMapBuilder[K, V]

  // scalac generates a `readReplace` method to discard the deserialized state (see https://github.com/scala/bug/issues/10412).
  // This prevents it from serializing it in the first place:
  private[this] def writeObject(out: ObjectOutputStream): Unit = ()
  private[this] def readObject(in: ObjectInputStream): Unit = ()
}


final class HashMapBuilder[K, V] extends Builder[(K, V), HashMap[K, V]] {
  import Node._
  import MapNode._

  private def newEmptyRootNode = new BitmapIndexedMapNode[K, V](0, 0, Array(), Array(), 0)

  /** True if and only if `rootNode` has been given out as a return value of `result()`.
    * Indicates that on next add, the elements should be copied to an identical structure, before continuing
    * mutations. */
  private var aliased: Boolean = false

  /** The root node of the partially build hashmap */
  private var rootNode: MapNode[K, V] = newEmptyRootNode

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
  private def insertValue[V1 >: V](bm: BitmapIndexedMapNode[K, V],bitpos: Int, key: K, originalHash: Int, keyHash: Int, value: V1): Unit = {
    val dataIx = bm.dataIndex(bitpos)
    val idx = TupleLength * dataIx

    val src = bm.content
    val dst = new Array[Any](src.length + TupleLength)

    // copy 'src' and insert 2 element(s) at position 'idx'
    arraycopy(src, 0, dst, 0, idx)
    dst(idx) = key
    dst(idx + 1) = value
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
  private def migrateFromInlineToNode(bm: BitmapIndexedMapNode[K, V], bitpos: Int, node: MapNode[K, V]): Unit = {
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
  private def setValue[V1 >: V](bm: BitmapIndexedMapNode[K, V], bitpos: Int, newKey: K, newValue: V1): Unit = {
    val dataIx = bm.dataIndex(bitpos)
    val idx = TupleLength * dataIx
    bm.content(idx) = newKey
    bm.content(idx + 1) = newValue
  }

  /** Upserts a key/value pair into mapNode, mutably */
  private def update(mapNode: MapNode[K, V], key: K, value: V, originalHash: Int, keyHash: Int, shift: Int): Unit = {
    mapNode match {
      case bm: BitmapIndexedMapNode[K, V] =>
        val mask = maskFrom(keyHash, shift)
        val bitpos = bitposFrom(mask)
        if ((bm.dataMap & bitpos) != 0) {
          val index = indexFrom(bm.dataMap, mask, bitpos)
          val key0 = bm.getKey(index)

          if (key0 == key) {
            val value0 = mapNode.getValue(index)
            if (!((key0.asInstanceOf[AnyRef] eq key.asInstanceOf[AnyRef]) &&
              (value0.asInstanceOf[AnyRef] eq value.asInstanceOf[AnyRef]))) {
              setValue(bm, bitpos, key, value)
            }
          } else {

            val value0 = bm.getValue(index)
            val key0UnimprovedHash = key0.##
            val key0Hash = improve(key0UnimprovedHash)

            val subNodeNew: MapNode[K, V] =
              bm.mergeTwoKeyValPairs(key0, value0, key0UnimprovedHash, key0Hash, key, value, originalHash, keyHash, shift + BitPartitionSize)

            hash += keyHash
            migrateFromInlineToNode(bm, bitpos, subNodeNew)
          }

        } else if ((bm.nodeMap & bitpos) != 0) {
          val index = indexFrom(bm.nodeMap, mask, bitpos)
          val subNode = bm.getNode(index)
          val beforeSize = subNode.size
          update(subNode, key, value, originalHash, keyHash, shift + BitPartitionSize)
          bm.size += subNode.size - beforeSize
        } else {
          insertValue(bm, bitpos, key, originalHash, keyHash, value)
          hash += keyHash
        }
      case hc: HashCollisionMapNode[K, V] =>
        val index = hc.contentKeys.indexOf(key)
        if (index < 0) {
          hash += keyHash
          hc.contentKeys = hc.contentKeys.appended(key)
          hc.contentValues = hc.contentValues.appended(value)
        } else {
          hc.contentKeys(index) = key
          hc.contentValues(index) = value
        }
    }
  }
  /** Inserts the this key/value only if the key is not present in the map already */
  private def updateIfNotExists(mapNode: MapNode[K, V], key: K, value: V, originalHash: Int, keyHash: Int, shift: Int): Unit = {
    mapNode match {
      case bm: BitmapIndexedMapNode[K, V] =>
        val mask = maskFrom(keyHash, shift)
        val bitpos = bitposFrom(mask)
        if ((bm.dataMap & bitpos) != 0) {
          val index = indexFrom(bm.dataMap, mask, bitpos)
          val key0 = bm.getKey(index)

          if (key0 == key) {
            () // do nothing
          } else {

            val value0 = bm.getValue(index)
            val key0UnimprovedHash = key0.##
            val key0Hash = improve(key0UnimprovedHash)

            val subNodeNew: MapNode[K, V] =
              bm.mergeTwoKeyValPairs(key0, value0, key0UnimprovedHash, key0Hash, key, value, originalHash, keyHash, shift + BitPartitionSize)

            hash += keyHash
            migrateFromInlineToNode(bm, bitpos, subNodeNew)
          }

        } else if ((bm.nodeMap & bitpos) != 0) {
          val index = indexFrom(bm.nodeMap, mask, bitpos)
          val subNode = bm.getNode(index)
          val beforeSize = subNode.size
          updateIfNotExists(subNode, key, value, originalHash, keyHash, shift + BitPartitionSize)
          bm.size += subNode.size - beforeSize
        } else {
          insertValue(bm, bitpos, key, originalHash, keyHash, value)
          hash += keyHash
        }
      case hc: HashCollisionMapNode[K, V] =>
        val index = hc.contentKeys.indexOf(key)
        if (index < 0) {
          hash += keyHash
          hc.contentKeys = hc.contentKeys.appended(key)
          hc.contentValues = hc.contentValues.appended(value)
        }
    }
  }


  /** If currently referencing aliased structure, copy elements to new mutable structure */
  private def ensureUnaliased() = {
    if (aliased) copyElems()
    aliased = false
  }

  /** Copy elements to new mutable structure */
  private def copyElems(): Unit = {
    val temp = rootNode
    clear()
    temp.foreach(addOne)
  }

  override def result(): HashMap[K, V] =
    if (rootNode.size == 0) {
      HashMap.empty
    } else {
      aliased = true
      val hm = new HashMap(rootNode, hash)
      releaseFence()
      hm
    }

  override def addOne(elem: (K, V)): this.type = {
    ensureUnaliased()
    val h = elem._1.##
    val im = improve(h)
    update(rootNode, elem._1, elem._2, h, im, 0)
    this
  }


  private[collection] def addOneIfNotExists(key: K, value: V): this.type = {
    ensureUnaliased()
    val h = key.##
    val im = improve(h)
    updateIfNotExists(rootNode, key, value, h, im, 0)
    this
  }

  override def addAll(xs: IterableOnce[(K, V)]) = {
    ensureUnaliased()
    xs match {
      case hm: HashMap[K, V] =>
        def addAllRec(node: MapNode[K, V]): Unit = node match {
          case bm: BitmapIndexedMapNode[K, V] =>
            var i = 0
            val payloadArity = bm.payloadArity
            while (i < payloadArity) {
              val originalHash = bm.getHash(i)
              update(
                mapNode = rootNode,
                key = bm.getKey(i),
                value = bm.getValue(i),
                originalHash = originalHash,
                keyHash = improve(originalHash),
                0
              )
              i += 1
            }

            var j = 0
            while (j < bm.nodeArity) {
              addAllRec(bm.getNode(j))
              j += 1
            }

          case hc: HashCollisionMapNode[K, V] =>
            var i = 0
            while (i < hc.contentKeys.length) {
              update(rootNode, hc.getKey(i), hc.getValue(i), hc.originalHash, hc.hash, shift = 0)
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
      rootNode = newEmptyRootNode
    }
    hash = 0
  }

  private[collection] def size: Int = rootNode.size
}