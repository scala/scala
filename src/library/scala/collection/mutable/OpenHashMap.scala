/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection
package mutable

/**
 *  @define Coll `OpenHashMap`
 *  @define coll open hash map
 *
 *  @since 2.7
 */
object OpenHashMap {
  def apply[K, V](elems : (K, V)*) = new OpenHashMap[K, V] ++= elems
  def empty[K, V] = new OpenHashMap[K, V]

  final private class OpenEntry[Key, Value](val key: Key,
                                            val hash: Int,
                                            var value: Value)

  private[mutable] def nextPositivePowerOfTwo(i : Int) = 1 << (32 - Integer.numberOfLeadingZeros(i - 1))

  // Hash table slot states

  /** The hash table slot is empty. */
  private val Empty: Byte = 0
  /** The hash table slot was deleted. */
  private val Deleted: Byte = 1
  /** The hash table slot is occupied. */
  private val Occupied: Byte = 2
}

/** A mutable hash map based on an open hashing scheme. The precise scheme is
 *  undefined, but it should make a reasonable effort to ensure that an insert
 *  with consecutive hash codes is not unnecessarily penalised. In particular,
 *  mappings of consecutive integer keys should work without significant
 *  performance loss.
 *
 *  @tparam Key          type of the keys in this map.
 *  @tparam Value        type of the values in this map.
 *  @param initialSize   the initial size of the internal hash table.
 *
 *  @author David MacIver
 *  @since  2.7
 *
 *  @define Coll `OpenHashMap`
 *  @define coll open hash map
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
class OpenHashMap[Key, Value](initialSize : Int)
extends AbstractMap[Key, Value]
   with Map[Key, Value]
   with MapLike[Key, Value, OpenHashMap[Key, Value]] {

  import OpenHashMap._
  private type Entry = OpenEntry[Key, Value]

  /** A default constructor creates a hashmap with initial size `8`.
   */
  def this() = this(8)

  override def empty: OpenHashMap[Key, Value] = OpenHashMap.empty[Key, Value]

  private[this] val actualInitialSize = OpenHashMap.nextPositivePowerOfTwo(initialSize)

  private[this] var mask = actualInitialSize - 1

  /** The state of each of the slots in the hash table.
    * One of `Empty`, `Deleted` or `Occupied`.
    */
  private[this] var slotStates = new Array[Byte](actualInitialSize)

  /** The hash table.
    * 
    * The table's entries are initialized to `null`, an alternative indication of an empty slot.
    * A slot is either deleted or occupied if the entry is non-`null`.
    */
  private[this] var table = new Array[Entry](actualInitialSize)

  /** The default value of a `Value`. */
  private[this] val defaultValue = null.asInstanceOf[Value]

  private[this] var _size = 0
  private[this] var deleted = 0

  // Used for tracking inserts so that iterators can determine in concurrent modification has occurred.
  private[this] var modCount = 0

  override def size = _size
  private[this] def size_=(s : Int) { _size = s }

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
    mask = newSize - 1

    val oldSlotStates = slotStates
    slotStates = new Array(newSize)
    val oldTable = table
    table = new Array(newSize)

    var i = 0
    while (i < oldSlotStates.length) {
      if (oldSlotStates(i) == Occupied) {
        val entry = oldTable(i)
        val index = findIndex(entry.key, entry.hash)
        table(index) = entry
        slotStates(index) = Occupied
      }
      i += 1
    }

    deleted = 0
  }

  /** Return the index of the first slot in the hash table (in probe order)
    * that either is empty, or is or was last occupied by the given key.
    */
  private[this] def findIndex(key: Key) : Int = findIndex(key, hashOf(key))

  /** Return the index of the first slot in the hash table (in probe order)
    * that either is empty, or is or was last occupied by the given key.
    * 
    * This method is an optimization for when the hash value is in hand.
    * 
    * @param hash hash value for `key`
    */
  private[this] def findIndex(key: Key, hash: Int): Int = {
    var index = hash & mask
    var j = 0
    while(table(index) != null &&
          !(table(index).hash == hash &&
            table(index).key == key)){
      j += 1
      index = (index + j) & mask
    }
    index
  }

  override def update(key: Key, value: Value) {
    put(key, hashOf(key), value)
  }

  @deprecatedOverriding("+= should not be overridden in order to maintain consistency with put.", "2.11.0")
  def += (kv: (Key, Value)): this.type = { put(kv._1, kv._2); this }
  
  @deprecatedOverriding("-= should not be overridden in order to maintain consistency with remove.", "2.11.0")
  def -= (key: Key): this.type = { remove(key); this }

  override def put(key: Key, value: Value): Option[Value] =
    put(key, hashOf(key), value)

  private def put(key: Key, hash: Int, value: Value): Option[Value] = {
    if (2 * (size + deleted) > mask) growTable()
    val index = findIndex(key, hash)
    val state = slotStates(index)
    slotStates(index) = Occupied
    if (state == Empty) {
      table(index) = new OpenEntry(key, hash, value)
      modCount += 1
      size += 1
      None
    } else {
      val entry = table(index)
      if (state == Deleted) {
        size += 1
        deleted -= 1
        modCount += 1
        entry.value = value
        None
      } else {
        val res = entry.value
        entry.value = value
        Some(res)
      }
    }
  }

  override def remove(key : Key): Option[Value] = {
    val index = findIndex(key)
    if (slotStates(index) == Occupied) {
      val res = table(index).value
      table(index).value = defaultValue
      slotStates(index) = Deleted
      size -= 1
      deleted += 1
      Some(res)
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
        return if (slotStates(index) == Occupied) Some(entry.value) else None
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
  def iterator: Iterator[(Key, Value)] = new AbstractIterator[(Key, Value)] {
    var index = 0
    val initialModCount = modCount

    private[this] def advance() {
      if (initialModCount != modCount) sys.error("Concurrent modification")
      while (index <= mask && slotStates(index) != Occupied)  index += 1
    }

    def hasNext = {advance(); index <= mask }

    def next = {
      advance()
      val result = table(index)
      index += 1
      (result.key, result.value)
    }
  }

  override def clone() = {
    val it = new OpenHashMap[Key, Value]
    foreachOccupiedEntry(entry => it.put(entry.key, entry.hash, entry.value))
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
  override def foreach[U](f : ((Key, Value)) => U) {
    val startModCount = modCount
    foreachOccupiedEntry(entry => {
      if (modCount != startModCount) sys.error("Concurrent Modification")
      f((entry.key, entry.value))}
    )
  }

  /** Call the given function on every occupied hash table entry. */
  private[this] def foreachOccupiedEntry(f: Entry => Unit) {
    var i = 0
    while (i < slotStates.length) {
      if (slotStates(i) == Occupied)  f(table(i))
      i += 1
    }
  }

  override def transform(f : (Key, Value) => Value) = {
    foreachOccupiedEntry(entry => entry.value = f(entry.key, entry.value))
    this
  }

  override def retain(f : (Key, Value) => Boolean) = {
    var i = 0
    while (i < slotStates.length) {
      if (slotStates(i) == Occupied) {
        val entry = table(i)
        if (!f(entry.key, entry.value)) {
          entry.value = defaultValue
          slotStates(i) = Deleted
          size -= 1
          deleted += 1
        }
      }
      i += 1
    }

    this
  }

  override def stringPrefix = "OpenHashMap"
}
