/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

/**
 * @since 2.7
 */
object OpenHashMap{
  def apply[K, V](elems : (K, V)*) = {
    val dict = new OpenHashMap[K, V];
    elems.foreach({case (x, y) => dict(x) = y});
    dict;
  }

  def empty[K, V] = new OpenHashMap[K, V];

  private[mutable] class Entry[Key, Value](val key : Key,
                                           val hash : Int,
                                           var value : Option[Value])

  private[mutable] def highestOneBit(j : Int) = { // This should really go somewhere central as we're now code sharing by cut and paste. :(
    var i = j;
    i |= (i >>  1);
    i |= (i >>  2);
    i |= (i >>  4);
    i |= (i >>  8);
    i |= (i >> 16);
    i - (i >>> 1);
  }

  private[mutable] def nextPowerOfTwo(i : Int) = highestOneBit(i) << 1;
}

import OpenHashMap.Entry;

/**
 * A mutable hash map based on an open hashing scheme. The precise scheme is undefined,
 * but it should make a reasonable effort to ensure that an insert with consecutive hash
 * codes is not unneccessarily penalised. In particular, mappings of consecutive integer
 * keys should work without significant performance loss.
 *
 * @author David MacIver
 * @since  2.7
 */
class OpenHashMap[Key, Value](initialSize : Int) extends scala.collection.mutable.Map[Key, Value]{
  def this() = this(8);

  override def empty = OpenHashMap.empty

  private[this] val actualInitialSize = OpenHashMap.nextPowerOfTwo(initialSize);

  private var mask = actualInitialSize - 1;;
  private var table : Array[Entry[Key, Value]] = new Array[Entry[Key, Value]](actualInitialSize);
  private var _size = 0;
  private var deleted = 0;

  // Used for tracking inserts so that iterators can determine in concurrent modification has occurred.
  private[this] var modCount = 0;

  override def size = _size;
  private[this] def size_=(s : Int) = _size = s;

  protected def hashOf(key : Key) = {
    var h = key.hashCode;
    h ^= ((h >>> 20) ^ (h >>> 12));
    h ^ (h >>> 7) ^ (h >>> 4);
  }

  private[this] def growTable = {
    val oldSize = mask + 1;
    val newSize = 4 * oldSize;
    val oldTable = table;
    table = new Array[Entry[Key, Value]](newSize);
    mask = newSize - 1;
    oldTable.foreach( entry =>
      if (entry != null && entry.value != None) addEntry(entry));
    deleted = 0;
  }

  private[this] def findIndex(key : Key) : Int = findIndex(key, hashOf(key));

  private[this] def findIndex(key : Key, hash : Int) : Int = {
    var j = hash;

    var index = hash & mask;
    var perturb = index;
    while(table(index) != null &&
          !(table(index).hash == hash &&
            table(index).key == key)){
      j = 5 * j + 1 + perturb;
      perturb >>= 5;
      index = j & mask;
    }
    index;
  }

  private[this] def addEntry(entry : Entry[Key, Value]) =
    if (entry != null) table(findIndex(entry.key, entry.hash)) = entry;

  override def update(key : Key, value : Value) {
    put(key, hashOf(key), value);
  }

  def += (kv: (Key, Value)): this.type = { put(kv._1, kv._2); this }
  def -= (key: Key): this.type = { remove(key); this }

  override def put(key : Key, value : Value): Option[Value] =
    put(key, hashOf(key), value)

  private def put(key : Key, hash : Int, value : Value): Option[Value] = {
    if (2 * (size + deleted) > mask) growTable;
    val index = findIndex(key, hash);
    val entry = table(index);
    if (entry == null) {
      table(index) = new Entry(key, hash, Some(value));
      modCount += 1;
      size += 1;
      None
    } else {
      val res = entry.value
      if (entry.value == None) { size += 1; modCount += 1 }
      entry.value = Some(value);
      res
    }
  }

  override def remove(key : Key): Option[Value] = {
    val index = findIndex(key);
    if (table(index) != null && table(index).value != None){
      val res = table(index).value
      table(index).value = None;
      size -= 1;
      deleted += 1;
      res
    } else None
  }

  def get(key : Key) : Option[Value] = {
    val hash = hashOf(key);

    var j = hash;
    var index = hash & mask;
    var perturb = index;
    var entry = table(index);
    while(entry != null){
      if (entry.hash == hash &&
          entry.key == key){
        return entry.value;
      }

      j = 5 * j + 1 + perturb;
      perturb >>= 5;
      index = j & mask;
      entry = table(index);
    }
    None;
  }

  /**
   * An iterator over the elements of this map. Use of this iterator follows the same
   * contract for concurrent modification as the foreach method.
   */
  def iterator = new Iterator[(Key, Value)]{
    var index = 0;
    val initialModCount = modCount;

    private[this] def advance {
      if (initialModCount != modCount) error("Concurrent modification");
      while((index <= mask) && (table(index) == null || table(index).value == None)) index+=1;
    }

    def hasNext = {advance; index <= mask; }

    def next = {
      advance;
      val result = table(index);
      index += 1;
      (result.key, result.value.get);
    }
  }

  override def clone : OpenHashMap[Key, Value] = {
    val it = new OpenHashMap[Key, Value]
    foreachUndeletedEntry(entry => it.put(entry.key, entry.hash, entry.value.get));
    it
  }

  /**
   * Loop over the key, value mappings of this map.
   *
   * The behaviour of modifying the map during an iteration is as follows:
   *
   * <ul>
   *  <li>Deleting a mapping is always permitted.</li>
   *  <li>Changing the value of mapping which is already present is permitted.</li>
   *  <li>Anything else is not permitted. It will usually, but not always, throw an exception.</li>
   * </ul>
   *
   * @param  f The function to apply to each key, value mapping.
   */
  override def foreach[U](f : ((Key, Value)) =>  U){
    val startModCount = modCount;
    foreachUndeletedEntry(entry => {
      if (modCount != startModCount) error("Concurrent Modification")
      f((entry.key, entry.value.get))}
    );
  }

  private[this] def foreachUndeletedEntry(f : Entry[Key, Value] => Unit){
    table.foreach(entry => if (entry != null && entry.value != None) f(entry));
  }
  override def transform(f : (Key, Value) => Value) = {
    foreachUndeletedEntry(entry => entry.value = Some(f(entry.key, entry.value.get)));
    this
  }

  override def retain(f : (Key, Value) => Boolean) = {
    foreachUndeletedEntry(entry => if (!f(entry.key, entry.value.get)) {entry.value = None; size -= 1; deleted += 1} );
    this
  }

  override def stringPrefix = "OpenHashMap"
}
