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
  protected def initialThreshold: Int = newThreshold(initialSize)

  /** The actual hash table.
   */
  protected var table: Array[HashEntry[A, Entry]] =
    if (initialSize == 0) null else new Array(initialSize)

  /** The number of mappings contained in this hash table.
   */
  protected var tableSize: Int = 0

  /** The next size value at which to resize (capacity * load factor).
   */
  protected var threshold: Int = initialThreshold

  /** Returns the size of this hash table.
   */
  def size = tableSize

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

  /** An iterator returning all entries */
  @deprecated("use entriesIterator instead")
  protected def entries: Iterator[Entry] = entriesIterator

  /** Remove all entries from table
   */
  def clear() {
    var i = table.length - 1
    while (i >= 0) { table(i) = null; i = i - 1 }
    tableSize = 0
  }

  private def newThreshold(size: Int) =
    ((size.toLong * loadFactor)/loadFactorDenum).toInt

  private def resize(newSize: Int) = {
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


