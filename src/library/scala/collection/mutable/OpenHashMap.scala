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

package scala.collection
package mutable

import java.lang.Integer.numberOfLeadingZeros
import java.util.ConcurrentModificationException
import scala.collection.generic.DefaultSerializable

/**
  *  @define Coll `OpenHashMap`
  *  @define coll open hash map
  */
@deprecated("Use HashMap or one of the specialized versions (LongMap, AnyRefMap) instead of OpenHashMap", "2.13.0")
@SerialVersionUID(3L)
object OpenHashMap extends MapFactory[OpenHashMap] {

  def empty[K, V] = new OpenHashMap[K, V]
  def from[K, V](it: IterableOnce[(K, V)]): OpenHashMap[K,V] = empty ++= it

  def newBuilder[K, V]: Builder[(K, V), OpenHashMap[K,V]] =
    new GrowableBuilder[(K, V), OpenHashMap[K, V]](empty)

  /** A hash table entry.
    *
    * The entry is occupied if and only if its `value` is a `Some`;
    * deleted if and only if its `value` is `None`.
    * If its `key` is not the default value of type `Key`, the entry is occupied.
    * If the entry is occupied, `hash` contains the hash value of `key`.
    */
  final private class OpenEntry[Key, Value](var key: Key,
                                            var hash: Int,
                                            var value: Option[Value])

  private[mutable] def nextPositivePowerOfTwo(target: Int): Int = 1 << -numberOfLeadingZeros(target - 1)
}

/** A mutable hash map based on an open addressing method. The precise scheme is
  *  undefined, but it should make a reasonable effort to ensure that an insert
  *  with consecutive hash codes is not unnecessarily penalised. In particular,
  *  mappings of consecutive integer keys should work without significant
  *  performance loss.
  *
  *  @tparam Key          type of the keys in this map.
  *  @tparam Value        type of the values in this map.
  *  @param initialSize   the initial size of the internal hash table.
  *
  *  @define Coll `OpenHashMap`
  *  @define coll open hash map
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */
@deprecated("Use HashMap or one of the specialized versions (LongMap, AnyRefMap) instead of OpenHashMap", "2.13.0")
class OpenHashMap[Key, Value](initialSize : Int)
  extends AbstractMap[Key, Value]
    with MapOps[Key, Value, OpenHashMap, OpenHashMap[Key, Value]]
    with StrictOptimizedIterableOps[(Key, Value), Iterable, OpenHashMap[Key, Value]]
    with MapFactoryDefaults[Key, Value, OpenHashMap, Iterable]
    with DefaultSerializable {

  import OpenHashMap.OpenEntry
  private type Entry = OpenEntry[Key, Value]

  /** A default constructor creates a hashmap with initial size `8`.
    */
  def this() = this(8)

  override def mapFactory: MapFactory[OpenHashMap] = OpenHashMap

  private[this] val actualInitialSize = OpenHashMap.nextPositivePowerOfTwo(initialSize)

  private[this] var mask = actualInitialSize - 1

  /** The hash table.
    *
    * The table's entries are initialized to `null`, indication of an empty slot.
    * A slot is either deleted or occupied if and only if the entry is non-`null`.
    */
  private[this] var table = new Array[Entry](actualInitialSize)

  private[this] var _size = 0
  private[this] var deleted = 0

  // Used for tracking inserts so that iterators can determine if concurrent modification has occurred.
  private[this] var modCount = 0

  override def size = _size
  override def knownSize: Int = size
  private[this] def size_=(s : Int): Unit = _size = s
  override def isEmpty: Boolean = _size == 0
  /** Returns a mangled hash code of the provided key. */
  protected def hashOf(key: Key) = {
    var h = key.##
    h ^= ((h >>> 20) ^ (h >>> 12))
    h ^ (h >>> 7) ^ (h >>> 4)
  }

  /** Increase the size of the table.
    * Copy only the occupied slots, effectively eliminating the deleted slots.
    */
  private[this] def growTable() = {
    val oldSize = mask + 1
    val newSize = 4 * oldSize
    val oldTable = table
    table = new Array[Entry](newSize)
    mask = newSize - 1
    oldTable.foreach( entry =>
      if (entry != null && entry.value != None)
        table(findIndex(entry.key, entry.hash)) = entry )
    deleted = 0
  }

  /** Return the index of the first slot in the hash table (in probe order)
    * that is, in order of preference, either occupied by the given key, deleted, or empty.
    *
    * @param hash hash value for `key`
    */
  private[this] def findIndex(key: Key, hash: Int): Int = {
    var index = hash & mask
    var j = 0

    // Index of the first slot containing a deleted entry, or -1 if none found yet
    var firstDeletedIndex = -1

    var entry = table(index)
    while (entry != null) {
      if (entry.hash == hash && entry.key == key && entry.value != None)
        return index

      if (firstDeletedIndex == -1 && entry.value == None)
        firstDeletedIndex = index

      j += 1
      index = (index + j) & mask
      entry = table(index)
    }

    if (firstDeletedIndex == -1) index else firstDeletedIndex
  }

  // TODO refactor `put` to extract `findOrAddEntry` and implement this in terms of that to avoid Some boxing.
  override def update(key: Key, value: Value): Unit = put(key, value)

  @deprecatedOverriding("addOne should not be overridden in order to maintain consistency with put.", "2.11.0")
  def addOne (kv: (Key, Value)): this.type = { put(kv._1, kv._2); this }

  @deprecatedOverriding("subtractOne should not be overridden in order to maintain consistency with remove.", "2.11.0")
  def subtractOne (key: Key): this.type = { remove(key); this }

  override def put(key: Key, value: Value): Option[Value] =
    put(key, hashOf(key), value)

  private def put(key: Key, hash: Int, value: Value): Option[Value] = {
    if (2 * (size + deleted) > mask) growTable()
    val index = findIndex(key, hash)
    val entry = table(index)
    if (entry == null) {
      table(index) = new OpenEntry(key, hash, Some(value))
      modCount += 1
      size += 1
      None
    } else {
      val res = entry.value
      if (entry.value == None) {
        entry.key = key
        entry.hash = hash
        size += 1
        deleted -= 1
        modCount += 1
      }
      entry.value = Some(value)
      res
    }
  }

  /** Delete the hash table slot contained in the given entry. */
  @`inline`
  private[this] def deleteSlot(entry: Entry) = {
    entry.key = null.asInstanceOf[Key]
    entry.hash = 0
    entry.value = None

    size -= 1
    deleted += 1
  }

  override def remove(key : Key): Option[Value] = {
    val entry = table(findIndex(key, hashOf(key)))
    if (entry != null && entry.value != None) {
      val res = entry.value
      deleteSlot(entry)
      res
    } else None
  }

  def get(key : Key) : Option[Value] = {
    val hash = hashOf(key)
    var index = hash & mask
    var entry = table(index)
    var j = 0
    while(entry != null){
      if (entry.hash == hash &&
        entry.key == key){
        return entry.value
      }

      j += 1
      index = (index + j) & mask
      entry = table(index)
    }
    None
  }

  /** An iterator over the elements of this map. Use of this iterator follows
    *  the same contract for concurrent modification as the foreach method.
    *
    *  @return   the iterator
    */
  def iterator: Iterator[(Key, Value)] = new OpenHashMapIterator[(Key, Value)] {
    override protected def nextResult(node: Entry): (Key, Value) = (node.key, node.value.get)
  }

  override def keysIterator: Iterator[Key] = new OpenHashMapIterator[Key] {
    override protected def nextResult(node: Entry): Key = node.key
  }
  override def valuesIterator: Iterator[Value] = new OpenHashMapIterator[Value] {
    override protected def nextResult(node: Entry): Value = node.value.get
  }

  private abstract class OpenHashMapIterator[A] extends AbstractIterator[A] {
    private[this] var index = 0
    private[this] val initialModCount = modCount

    private[this] def advance(): Unit = {
      if (initialModCount != modCount) throw new ConcurrentModificationException
      while((index <= mask) && (table(index) == null || table(index).value == None)) index+=1
    }

    def hasNext = {advance(); index <= mask }

    def next() = {
      advance()
      val result = table(index)
      index += 1
      nextResult(result)
    }
    protected def nextResult(node: Entry): A
  }

  override def clone() = {
    val it = new OpenHashMap[Key, Value]
    foreachUndeletedEntry(entry => it.put(entry.key, entry.hash, entry.value.get))
    it
  }

  /** Loop over the key, value mappings of this map.
    *
    *  The behaviour of modifying the map during an iteration is as follows:
    *  - Deleting a mapping is always permitted.
    *  - Changing the value of mapping which is already present is permitted.
    *  - Anything else is not permitted. It will usually, but not always, throw an exception.
    *
    *  @tparam U  The return type of the specified function `f`, return result of which is ignored.
    *  @param f   The function to apply to each key, value mapping.
    */
  override def foreach[U](f : ((Key, Value)) => U): Unit = {
    val startModCount = modCount
    foreachUndeletedEntry(entry => {
      if (modCount != startModCount) throw new ConcurrentModificationException
      f((entry.key, entry.value.get))}
    )
  }
  override def foreachEntry[U](f : (Key, Value) => U): Unit = {
    val startModCount = modCount
    foreachUndeletedEntry(entry => {
      if (modCount != startModCount) throw new ConcurrentModificationException
      f(entry.key, entry.value.get)}
    )
  }

  private[this] def foreachUndeletedEntry(f : Entry => Unit): Unit = {
    table.foreach(entry => if (entry != null && entry.value != None) f(entry))
  }

  override def mapValuesInPlace(f : (Key, Value) => Value): this.type = {
    foreachUndeletedEntry(entry => entry.value = Some(f(entry.key, entry.value.get)))
    this
  }

  override def filterInPlace(f : (Key, Value) => Boolean): this.type = {
    foreachUndeletedEntry(entry => if (!f(entry.key, entry.value.get)) deleteSlot(entry))
    this
  }

  override protected[this] def stringPrefix = "OpenHashMap"
}
