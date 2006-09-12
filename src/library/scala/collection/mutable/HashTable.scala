/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class can be used to construct data structures that are based
 *  on hashtables. Class <code>HashTable[A]</code> implements a hashtable
 *  that maps keys of type <code>A</code> to values of the fully abstract
 *  member type <code>Entry</code>. Classes that make use of <code>HashTable</code>
 *  have to provide an implementation for <code>Entry</code> and implement the
 *  function <code>entryKey</code>.<p/>
 *
 *  There are mainly two parameters that affect the performance of a hashtable:
 *  the <i>initial size</i> and the <i>load factor</i>. The <i>size</i>
 *  refers to the number of <i>buckets</i> in the hashtable, and the <i>load
 *  factor</i> is a measure of how full the hashtable is allowed to get before
 *  its size is automatically doubled. Both parameters may be changed by
 *  overriding the corresponding values in class <code>HashTable</code>.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
trait HashTable[A] extends AnyRef {

  /** The load factor for the hash table.
   */
  protected val loadFactor: Float = 0.75f

  /** The initial size of the hash table.
   */
  protected val initialSize: Int = 16

  /** The initial threshold
   */
  protected val initialThreshold: Int = newThreshold(initialSize)

  /** The actual hash table.
   */
  protected var table: Array[List[Entry]] = new Array(initialSize)
  initTable(table)

  /** The number of mappings contained in this hash table.
   */
  protected var tableSize: Int = 0

  /** The next size value at which to resize (capacity * load factor).
   */
  protected var threshold: Int = initialThreshold

  /** Returns the size of this hash map.
   */
  def size = tableSize

  protected def findEntry(key: A): Option[Entry] =
    table(index(elemHashCode(key))).find(entryFor(key))

  protected def addEntry(e: Entry): Unit = {
    val h = index(elemHashCode(entryKey(e)))
    table(h) = e :: table(h)
    tableSize = tableSize + 1
    if (tableSize > threshold)
      resize(2 * table.length)
  }

  protected def removeEntry(key: A): Unit = findEntry(key) match {
    case None =>
    case Some(e) =>
      val idx = index(elemHashCode(key))
      table(idx) = table(idx).filter(e => !elemEquals(entryKey(e), key))
      tableSize = tableSize - 1
  }

  protected type Entry

  protected def entryKey(e: Entry): A

  protected def entries: Iterator[Entry] = new Iterator[Entry] {
    val iterTable = table
    var idx = table.length - 1
    var xs = iterTable(idx)
    scan()
    def hasNext = !xs.isEmpty
    def next = {
      val res = xs.head
      xs = xs.tail
      scan()
      res
    }
    def scan(): Unit = if (xs.isEmpty && (idx > 0)) {
      idx = idx - 1
      xs = iterTable(idx)
      scan()
    }
  }

  private def entryFor(key: A) = { e: Entry => elemEquals(entryKey(e), key) }

  protected def initTable(tb: Array[List[Entry]]): Unit = {
    var i = tb.length - 1
    while (i >= 0) {
      tb(i) = Nil
      i = i - 1
    }
  }

  private def newThreshold(size: Int) =
    (size * loadFactor).asInstanceOf[Int]

  private def resize(newSize: Int) = {
    val newTable: Array[List[Entry]] = new Array(newSize)
    initTable(newTable)
    var i = table.length - 1
    while (i >= 0) {
      table(i).foreach { e => {
        val idx = improve(elemHashCode(entryKey(e))) & (newSize - 1)
        newTable(idx) = e :: newTable(idx)
      }}
      i = i - 1
    }
    table = newTable
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
