/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://www.scala-lang.org/           **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

/** This class can be used to construct data structures that are based
 *  on hashtables. Class <code>HashTable[A]</code> implements a hashtable
 *  that maps keys of type <code>A</code> to values of the fully abstract
 *  member type <code>Entry</code>. Classes that make use of <code>HashTable</code>
 *  have to provide an implementation for <code>Entry</code>
 *
 *  There are mainly two parameters that affect the performance of a hashtable:
 *  the <i>initial size</i> and the <i>load factor</i>. The <i>size</i>
 *  refers to the number of <i>buckets</i> in the hashtable, and the <i>load
 *  factor</i> is a measure of how full the hashtable is allowed to get before
 *  its size is automatically doubled. Both parameters may be changed by
 *  overriding the corresponding values in class <code>HashTable</code>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 *  @since   1
 */
trait HashTable[A] {
  import HashTable._

  protected type Entry >: Null <: HashEntry[A, Entry]

  /** The load factor for the hash table (in 0.001 step).
   */
  protected def loadFactor: Int = 750 // corresponds to 75%
  protected final val loadFactorDenum = 1000;

  /** The initial size of the hash table.
   */
  protected def initialSize: Int = 16

  /** The initial threshold
   */
  protected def initialThreshold: Int = newThreshold(initialCapacity)

  @transient private[collection] var _loadFactor = loadFactor

  /** The actual hash table.
   */
  @transient protected var table: Array[HashEntry[A, Entry]] = new Array(initialCapacity)

  /** The number of mappings contained in this hash table.
   */
  @transient protected var tableSize: Int = 0

  /** The next size value at which to resize (capacity * load factor).
   */
  @transient protected var threshold: Int = initialThreshold

  private def initialCapacity = capacity(initialSize)

  /**
   * Initialises the collection from the input stream. `f` will be called for each key/value pair
   * read from the input stream in the order determined by the stream. This is useful for
   * structures where iteration order is important (e.g. LinkedHashMap).
   */
  private[collection] def init[B](in: java.io.ObjectInputStream, f: (A, B) => Entry) {
    in.defaultReadObject

    _loadFactor = in.readInt
    assert(_loadFactor > 0)

    val size = in.readInt
    assert(size >= 0)

    table = new Array(capacity(size * loadFactorDenum / _loadFactor))
    threshold = newThreshold(table.size)

    var index = 0
    while (index < size) {
      addEntry(f(in.readObject.asInstanceOf[A], in.readObject.asInstanceOf[B]))
      index += 1
    }
  }

  /**
   * Serializes the collection to the output stream by saving the load factor, collection
   * size, collection keys and collection values. `value` is responsible for providing a value
   * from an entry.
   *
   * `foreach` determines the order in which the key/value pairs are saved to the stream. To
   * deserialize, `init` should be used.
   */
  private[collection] def serializeTo[B](out: java.io.ObjectOutputStream, value: Entry => B) {
    out.defaultWriteObject
    out.writeInt(loadFactor)
    out.writeInt(tableSize)
    foreachEntry { entry =>
      out.writeObject(entry.key)
      out.writeObject(value(entry))
    }
  }

  private def capacity(expectedSize: Int) = if (expectedSize == 0) 1 else powerOfTwo(expectedSize)

  /** Find entry with given key in table, null if not found
   */
  protected def findEntry(key: A): Entry = {
    val h = index(elemHashCode(key))
    var e = table(h).asInstanceOf[Entry]
    while (e != null && !elemEquals(e.key, key)) e = e.next
    e
  }

  /** Add entry to table
   *  pre: no entry with same key exists
   */
  protected def addEntry(e: Entry) {
    val h = index(elemHashCode(e.key))
    e.next = table(h).asInstanceOf[Entry]
    table(h) = e
    tableSize = tableSize + 1
    if (tableSize > threshold)
      resize(2 * table.length)
  }

  /** Remove entry from table if present
   */
  protected def removeEntry(key: A) : Entry = {
    val h = index(elemHashCode(key))
    var e = table(h).asInstanceOf[Entry]
    if (e != null) {
      if (elemEquals(e.key, key)) {
        table(h) = e.next
        tableSize = tableSize - 1
        return e
      } else {
        var e1 = e.next
        while (e1 != null && !elemEquals(e1.key, key)) {
          e = e1
          e1 = e1.next
        }
        if (e1 != null) {
          e.next = e1.next
          tableSize = tableSize - 1
          return e1
        }
      }
    }
    null
  }

  /** An iterator returning all entries
   */
  protected def entriesIterator: Iterator[Entry] = new Iterator[Entry] {
    val iterTable = table
    var idx = table.length - 1
    var es = iterTable(idx).asInstanceOf[Entry]
    scan()
    def hasNext = es != null
    def next = {
      val res = es
      es = es.next
      scan()
      res
    }
    def scan() {
      while (es == null && idx > 0) {
        idx = idx - 1
        es = iterTable(idx).asInstanceOf[Entry]
      }
    }
  }

  /*
   * We should implement this as a primitive operation over the underlying array, but it can
   * cause a behaviour change in edge cases where:
   * - Someone modifies a map during iteration
   * - The insertion point is close to the iteration point.
   *
   * The reason this happens is that the iterator prefetches the following element before
   * returning from next (to simplify the implementation of hasNext) while the natural
   * implementation of foreach does not.
   *
   * It should be mentioned that modifying a map during iteration leads to unpredictable
   * results with either implementation.
   */
  protected final def foreachEntry[C](f: Entry => C) { entriesIterator.foreach(f) }

  /** An iterator returning all entries */
  @deprecated("use entriesIterator instead")
  protected def entries: Iterator[Entry] = entriesIterator

  /** Remove all entries from table
   */
  protected def clearTable() {
    var i = table.length - 1
    while (i >= 0) { table(i) = null; i = i - 1 }
    tableSize = 0
  }

  private def newThreshold(size: Int) =
    ((size.toLong * _loadFactor)/loadFactorDenum).toInt

  private def resize(newSize: Int) {
    val oldTable = table
    table = new Array(newSize)
    var i = oldTable.length - 1
    while (i >= 0) {
      var e = oldTable(i)
      while (e != null) {
        val h = index(elemHashCode(e.key))
        val e1 = e.next
        e.next = table(h).asInstanceOf[Entry]
        table(h) = e
        e = e1
      }
      i = i - 1
    }
    threshold = newThreshold(newSize)
  }

  protected def elemEquals(key1: A, key2: A): Boolean = (key1 == key2)

  protected def elemHashCode(key: A) = key.hashCode()

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  protected final def index(hcode: Int) = improve(hcode) & (table.length - 1)
}

private[collection] object HashTable {

  /**
   * Returns a power of two >= `target`.
   */
  private[collection] def powerOfTwo(target: Int): Int = {
    /* See http://bits.stephan-brumme.com/roundUpToNextPowerOfTwo.html */
    var c = target - 1;
    c |= c >>>  1;
    c |= c >>>  2;
    c |= c >>>  4;
    c |= c >>>  8;
    c |= c >>> 16;
    c + 1;
  }
}
