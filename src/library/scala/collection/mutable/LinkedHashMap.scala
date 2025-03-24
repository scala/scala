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

package scala
package collection
package mutable

import scala.annotation.{nowarn, tailrec}
import scala.collection.generic.DefaultSerializable
import scala.util.hashing.MurmurHash3


/** This class implements mutable maps using a hashtable.
 *  The iterator and all traversal methods of this class visit elements in the order they were inserted.
 *
 *  @tparam K    the type of the keys contained in this hash map.
 *  @tparam V    the type of the values assigned to keys in this hash map.
 *
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
@deprecatedInheritance("LinkedHashMap will be made final; use .withDefault for the common use case of computing a default value", "2.13.11")
class LinkedHashMap[K, V]
  extends AbstractMap[K, V]
    with SeqMap[K, V]
    with MapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with StrictOptimizedIterableOps[(K, V), Iterable, LinkedHashMap[K, V]]
    with StrictOptimizedMapOps[K, V, LinkedHashMap, LinkedHashMap[K, V]]
    with MapFactoryDefaults[K, V, LinkedHashMap, Iterable]
    with DefaultSerializable {

  override def mapFactory: MapFactory[LinkedHashMap] = LinkedHashMap

  // stepper / keyStepper / valueStepper are not overridden to use XTableStepper because that stepper
  // would not return the elements in insertion order

  private[collection] type Entry = LinkedHashMap.LinkedEntry[K, V]

  private[collection] def _firstEntry: Entry = firstEntry

  protected var firstEntry: Entry = null

  protected var lastEntry: Entry = null

  /* Uses the same implementation as mutable.HashMap. The hashtable holds the following invariant:
   * - For each i between 0 and table.length, the bucket at table(i) only contains keys whose hash-index is i.
   * - Every bucket is sorted in ascendant hash order
   * - The sum of the lengths of all buckets is equal to contentSize.
   */
  private[this] var table = new Array[Entry](tableSizeFor(LinkedHashMap.defaultinitialSize))

  private[this] var threshold: Int = newThreshold(table.length)

  private[this] var contentSize = 0

  override def last: (K, V) =
    if (size > 0) (lastEntry.key, lastEntry.value)
    else throw new NoSuchElementException("Cannot call .last on empty LinkedHashMap")

  override def lastOption: Option[(K, V)] =
    if (size > 0) Some((lastEntry.key, lastEntry.value))
    else None

  override def head: (K, V) =
    if (size > 0) (firstEntry.key, firstEntry.value)
    else throw new NoSuchElementException("Cannot call .head on empty LinkedHashMap")

  override def headOption: Option[(K, V)] =
    if (size > 0) Some((firstEntry.key, firstEntry.value))
    else None

  override def size = contentSize
  override def knownSize: Int = size
  override def isEmpty: Boolean = size == 0

  def get(key: K): Option[V] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }
  override def sizeHint(size: Int): Unit = {
    val target = tableSizeFor(((size + 1).toDouble / LinkedHashMap.defaultLoadFactor).toInt)
    if (target > table.length) growTable(target)
  }

  override def contains(key: K): Boolean = {
    if (getClass eq classOf[LinkedHashMap[_, _]])
      findEntry(key) != null
    else
      super.contains(key) // A subclass might override `get`, use the default implementation `contains`.
  }

  override def put(key: K, value: V): Option[V] = put0(key, value, getOld = true) match {
    case null => None
    case sm => sm
  }

  override def update(key: K, value: V): Unit = put0(key, value, getOld = false)

  override def remove(key: K): Option[V] = removeEntry0(key) match {
    case null => None
    case nd => Some(nd.value)
  }

  override def getOrElse[V1 >: V](key: K, default: => V1): V1 = {
    if (getClass != classOf[LinkedHashMap[_, _]]) {
      // subclasses of LinkedHashMap might customise `get` ...
      super.getOrElse(key, default)
    } else {
      // .. but in the common case, we can avoid the Option boxing.
      val nd = findEntry(key)
      if (nd eq null) default else nd.value
    }
  }

  override def getOrElseUpdate(key: K, defaultValue: => V): V = {
    if (getClass != classOf[LinkedHashMap[_, _]]) {
      // subclasses of LinkedHashMap might customise `get` ...
      super.getOrElseUpdate(key, defaultValue)
    } else {
      val hash = computeHash(key)
      val idx = index(hash)
      val nd = table(idx) match {
        case null => null
        case nd => nd.findEntry(key, hash)
      }
      if (nd != null) nd.value
      else {
        val table0 = table
        val default = defaultValue
        if (contentSize + 1 >= threshold) growTable(table.length * 2)
        // Avoid recomputing index if the `defaultValue()` or new element hasn't triggered a table resize.
        val newIdx = if (table0 eq table) idx else index(hash)
        put0(key, default, getOld = false, hash, newIdx)
        default
      }
    }
  }

  private[this] def removeEntry0(elem: K): Entry = removeEntry0(elem, computeHash(elem))

  /** Removes a key from this map if it exists
   *
   * @param elem the element to remove
   * @param hash the **improved** hashcode of `element` (see computeHash)
   * @return the node that contained element if it was present, otherwise null
   */
  private[this] def removeEntry0(elem: K, hash: Int): Entry = {
    val idx = index(hash)
    table(idx) match {
      case null => null
      case nd if nd.hash == hash && nd.key == elem =>
        // first element matches
        table(idx) = nd.next
        deleteEntry(nd)
        contentSize -= 1
        nd
      case nd =>
        // find an element that matches
        var prev = nd
        var next = nd.next
        while ((next ne null) && next.hash <= hash) {
          if (next.hash == hash && next.key == elem) {
            prev.next = next.next
            deleteEntry(next)
            contentSize -= 1
            return next
          }
          prev = next
          next = next.next
        }
        null
    }
  }

  /** Computes the improved hash of an original (`any.##`) hash. */
  @`inline` private[this] def improveHash(originalHash: Int): Int = {
    originalHash ^ (originalHash >>> 16)
  }
  @`inline` private[collection] def unimproveHash(improvedHash: Int): Int = improveHash(improvedHash)

  /** Computes the improved hash of this key */
  @`inline` private[this] def computeHash(o: K): Int = improveHash(o.##)

  @`inline` private[this] def index(hash: Int) = hash & (table.length - 1)

  @`inline` private[this] def findEntry(key: K): Entry = {
    val hash = computeHash(key)
    table(index(hash)) match {
      case null => null
      case nd => nd.findEntry(key, hash)
    }
  }

  def addOne(kv: (K, V)): this.type = {
    put(kv._1, kv._2)
    this
  }

  def subtractOne(key: K): this.type = {
    remove(key)
    this
  }

  private[this] abstract class LinkedHashMapIterator[T] extends AbstractIterator[T] {
    private[this] var cur = firstEntry
    def extract(nd: Entry): T
    def hasNext: Boolean = cur ne null
    def next(): T =
      if (hasNext) { val r = extract(cur); cur = cur.later; r }
      else Iterator.empty.next()
  }

  def iterator: Iterator[(K, V)] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[(K, V)] {
      def extract(nd: Entry): (K, V) = (nd.key, nd.value)
    }

  protected class LinkedKeySet extends KeySet {
    override def iterableFactory: IterableFactory[collection.Set] = LinkedHashSet
  }

  override def keySet: collection.Set[K] = new LinkedKeySet

  override def keysIterator: Iterator[K] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[K] {
      def extract(nd: Entry): K = nd.key
    }

  private[collection] def entryIterator: Iterator[Entry] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[Entry] {
      def extract(nd: Entry): Entry = nd
    }


  // Override updateWith for performance, so we can do the update while hashing
  // the input key only once and performing one lookup into the hash table
  override def updateWith(key: K)(remappingFunction: Option[V] => Option[V]): Option[V] = {
    if (getClass != classOf[LinkedHashMap[_, _]]) {
      // subclasses of LinkedHashMap might customise `get` ...
      super.updateWith(key)(remappingFunction)
    } else {
      val hash = computeHash(key)
      val indexedHash = index(hash)

      var foundEntry: Entry = null
      var previousEntry: Entry = null
      table(indexedHash) match {
        case null =>
        case nd =>
          @tailrec
          def findEntry(prev: Entry, nd: Entry, k: K, h: Int): Unit = {
            if (h == nd.hash && k == nd.key) {
              previousEntry = prev
              foundEntry = nd
            }
            else if ((nd.next eq null) || (nd.hash > h)) ()
            else findEntry(nd, nd.next, k, h)
          }

          findEntry(null, nd, key, hash)
      }

      val previousValue = foundEntry match {
        case null => None
        case nd => Some(nd.value)
      }

      val nextValue = remappingFunction(previousValue)

      (previousValue, nextValue) match {
        case (None, None) => // do nothing

        case (Some(_), None) =>
          if (previousEntry != null) previousEntry.next = foundEntry.next
          else table(indexedHash) = foundEntry.next
          deleteEntry(foundEntry)
          contentSize -= 1

        case (None, Some(value)) =>
          val newIndexedHash =
            if (contentSize + 1 >= threshold) {
              growTable(table.length * 2)
              index(hash)
            } else indexedHash
          put0(key, value, getOld = false, hash, newIndexedHash)

        case (Some(_), Some(newValue)) => foundEntry.value = newValue
      }
      nextValue
    }
  }

  override def valuesIterator: Iterator[V] =
    if (size == 0) Iterator.empty
    else new LinkedHashMapIterator[V] {
      def extract(nd: Entry): V = nd.value
    }


  override def foreach[U](f: ((K, V)) => U): Unit = {
    var cur = firstEntry
    while (cur ne null) {
      f((cur.key, cur.value))
      cur = cur.later
    }
  }

  override def foreachEntry[U](f: (K, V) => U): Unit = {
    var cur = firstEntry
    while (cur ne null) {
      f(cur.key, cur.value)
      cur = cur.later
    }
  }

  override def clear(): Unit = {
    java.util.Arrays.fill(table.asInstanceOf[Array[AnyRef]], null)
    contentSize = 0
    firstEntry = null
    lastEntry = null
  }

  private[this] def tableSizeFor(capacity: Int) =
    (Integer.highestOneBit((capacity - 1).max(4)) * 2).min(1 << 30)

  private[this] def newThreshold(size: Int) = (size.toDouble * LinkedHashMap.defaultLoadFactor).toInt

  /*create a new entry. If table is empty(firstEntry is null), then the
  * new entry will be the firstEntry. If not, just set the new entry to
  * be the lastEntry.
  * */
  private[this] def createNewEntry(key: K, hash: Int, value: V): Entry = {
    val e = new Entry(key, hash, value)
    if (firstEntry eq null) firstEntry = e
    else {
      lastEntry.later = e
      e.earlier = lastEntry
    }
    lastEntry = e
    e
  }

  /** Delete the entry from the LinkedHashMap, set the `earlier` and `later` pointers correctly */
  private[this] def deleteEntry(e: Entry): Unit = {
    if (e.earlier eq null) firstEntry = e.later
    else e.earlier.later = e.later
    if (e.later eq null) lastEntry = e.earlier
    else e.later.earlier = e.earlier
    e.earlier = null
    e.later = null
    e.next = null
  }

  private[this] def put0(key: K, value: V, getOld: Boolean): Some[V] = {
    if (contentSize + 1 >= threshold) growTable(table.length * 2)
    val hash = computeHash(key)
    val idx = index(hash)
    put0(key, value, getOld, hash, idx)
  }

  private[this] def put0(key: K, value: V, getOld: Boolean, hash: Int, idx: Int): Some[V] = {
    table(idx) match {
      case null =>
        table(idx) = createNewEntry(key, hash, value)
      case old =>
        var prev: Entry = null
        var n = old
        while ((n ne null) && n.hash <= hash) {
          if (n.hash == hash && key == n.key) {
            val old = n.value
            n.value = value
            return if (getOld) Some(old) else null
          }
          prev = n
          n = n.next
        }
        val nnode = createNewEntry(key, hash, value)
        if (prev eq null) {
          nnode.next = old
          table(idx) = nnode
        } else {
          nnode.next = prev.next
          prev.next = nnode
        }
    }
    contentSize += 1
    null
  }

  private[this] def growTable(newlen: Int): Unit = {
    if (newlen < 0)
      throw new RuntimeException(s"new hash table size $newlen exceeds maximum")
    var oldlen = table.length
    threshold = newThreshold(newlen)
    if (size == 0) table = new Array(newlen)
    else {
      table = java.util.Arrays.copyOf(table, newlen)
      val preLow = new Entry(null.asInstanceOf[K], 0, null.asInstanceOf[V])
      val preHigh = new Entry(null.asInstanceOf[K], 0, null.asInstanceOf[V])
      // Split buckets until the new length has been reached. This could be done more
      // efficiently when growing an already filled table to more than double the size.
      while (oldlen < newlen) {
        var i = 0
        while (i < oldlen) {
          val old = table(i)
          if (old ne null) {
            preLow.next = null
            preHigh.next = null
            var lastLow = preLow
            var lastHigh = preHigh
            var n = old
            while (n ne null) {
              val next = n.next
              if ((n.hash & oldlen) == 0) { // keep low
                lastLow.next = n
                lastLow = n
              } else { // move to high
                lastHigh.next = n
                lastHigh = n
              }
              n = next
            }
            lastLow.next = null
            if (old ne preLow.next) table(i) = preLow.next
            if (preHigh.next ne null) {
              table(i + oldlen) = preHigh.next
              lastHigh.next = null
            }
          }
          i += 1
        }
        oldlen *= 2
      }
    }
  }

  override def hashCode: Int = {
    if (isEmpty) MurmurHash3.emptyMapHash
    else {
      val tupleHashIterator = new LinkedHashMapIterator[Any] {
        var hash: Int = 0
        override def hashCode: Int = hash
        override def extract(nd: Entry): Any = {
          hash = MurmurHash3.tuple2Hash(unimproveHash(nd.hash), nd.value.##)
          this
        }
      }
      MurmurHash3.unorderedHash(tupleHashIterator, MurmurHash3.mapSeed)
    }
  }
  @nowarn("""cat=deprecation&origin=scala\.collection\.Iterable\.stringPrefix""")
  override protected[this] def stringPrefix = "LinkedHashMap"
}

/** $factoryInfo
 *  @define Coll `LinkedHashMap`
 *  @define coll linked hash map
 */
@SerialVersionUID(3L)
object LinkedHashMap extends MapFactory[LinkedHashMap] {

  def empty[K, V] = new LinkedHashMap[K, V]

  def from[K, V](it: collection.IterableOnce[(K, V)]) = {
    val newlhm = empty[K, V]
    newlhm.sizeHint(it, delta = 0)
    newlhm.addAll(it)
    newlhm
  }

  def newBuilder[K, V]: GrowableBuilder[(K, V), LinkedHashMap[K, V]] = new GrowableBuilder(empty[K, V])

  /** Class for the linked hash map entry, used internally.
    */
  private[mutable] final class LinkedEntry[K, V](val key: K, val hash: Int, var value: V) {
    var earlier: LinkedEntry[K, V] = null
    var later: LinkedEntry[K, V] = null
    var next: LinkedEntry[K, V] = null

    @tailrec
    final def findEntry(k: K, h: Int): LinkedEntry[K, V] =
      if (h == hash && k == key) this
      else if ((next eq null) || (hash > h)) null
      else next.findEntry(k, h)
  }

  /** The default load factor for the hash table */
  private[collection] final def defaultLoadFactor: Double = 0.75

  /** The default initial capacity for the hash table */
  private[collection] final def defaultinitialSize: Int = 16
}
