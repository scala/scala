/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package mutable

/**
 * @since 2.3
 */
trait FlatHashTable[A] {

  /** The load factor for the hash table; must be < 500 (0.5)
   */
  protected def loadFactor: Int = 450
  protected final def loadFactorDenum = 1000

  /** The initial size of the hash table.
   */
  protected def initialSize: Int = 16

  private final val tableDebug = false

  @transient private[collection] var _loadFactor = loadFactor

  /** The actual hash table.
   */
  @transient protected var table: Array[AnyRef] = new Array(initialCapacity)

  /** The number of mappings contained in this hash table.
   */
  @transient protected var tableSize = 0

  /** The next size value at which to resize (capacity * load factor).
   */
  @transient protected var threshold: Int = newThreshold(initialCapacity)

  import HashTable.powerOfTwo
  private def capacity(expectedSize: Int) = if (expectedSize == 0) 1 else powerOfTwo(expectedSize)
  private def initialCapacity = capacity(initialSize)

  /**
   * Initialises the collection from the input stream. `f` will be called for each element
   * read from the input stream in the order determined by the stream. This is useful for
   * structures where iteration order is important (e.g. LinkedHashSet).
   *
   * The serialization format expected is the one produced by `serializeTo`.
   */
  private[collection] def init(in: java.io.ObjectInputStream, f: A => Unit) {
    in.defaultReadObject

    _loadFactor = in.readInt
    assert(_loadFactor > 0)

    val size = in.readInt
    assert(size >= 0)

    table = new Array(capacity(size * loadFactorDenum / _loadFactor))
    threshold = newThreshold(table.size)

    var index = 0
    while (index < size) {
      val elem = in.readObject.asInstanceOf[A]
      f(elem)
      addEntry(elem)
      index += 1
    }
  }

  /**
   * Serializes the collection to the output stream by saving the load factor, collection
   * size and collection elements. `foreach` determines the order in which the elements are saved
   * to the stream. To deserialize, `init` should be used.
   */
  private[collection] def serializeTo(out: java.io.ObjectOutputStream) {
    out.defaultWriteObject
    out.writeInt(_loadFactor)
    out.writeInt(tableSize)
    iterator.foreach(out.writeObject)
  }

  def findEntry(elem: A): Option[A] = {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry && entry != elem) {
      h = (h + 1) % table.length
      entry = table(h)
    }
    if (null == entry) None else Some(entry.asInstanceOf[A])
  }

  def containsEntry(elem: A): Boolean = {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry && entry != elem) {
      h = (h + 1) % table.length
      entry = table(h)
    }
    null != entry
  }

  /** Add entry if not yet in table
   *  Return whether a new entry was added
   */
  def addEntry(elem: A) : Boolean = {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry) {
      if (entry == elem) return false
      h = (h + 1) % table.length
      entry = table(h)
    }
    table(h) = elem.asInstanceOf[AnyRef]
    tableSize = tableSize + 1
    if (tableSize >= threshold) growTable()
    true
  }

  def removeEntry(elem: A) : Option[A] = {
    if (tableDebug) checkConsistent()
    def precedes(i: Int, j: Int) = {
      val d = table.length >> 1
      if (i <= j) j - i < d
      else i - j > d
    }
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry) {
      if (entry == elem) {
        var h0 = h
        var h1 = (h0 + 1) % table.length
        while (null != table(h1)) {
          val h2 = index(elemHashCode(table(h1).asInstanceOf[A]))
          //Console.println("shift at "+h1+":"+table(h1)+" with h2 = "+h2+"? "+(h2 != h1)+precedes(h2, h0)+table.length)
          if (h2 != h1 && precedes(h2, h0)) {
            //Console.println("shift "+h1+" to "+h0+"!")
            table(h0) = table(h1)
            h0 = h1
          }
          h1 = (h1 + 1) % table.length
        }
        table(h0) = null
        tableSize -= 1
        if (tableDebug) checkConsistent()
        return Some(entry.asInstanceOf[A])
      }
      h = (h + 1) % table.length
      entry = table(h)
    }
    None
  }

  def iterator = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < table.length && (null == table(i))) i += 1
      i < table.length
    }
    def next(): A =
      if (hasNext) { i += 1; table(i - 1).asInstanceOf[A] }
      else Iterator.empty.next
  }

  private def growTable() {
    val oldtable = table
    table = new Array[AnyRef](table.length * 2)
    tableSize = 0
    threshold = newThreshold(table.length)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (null != entry) addEntry(entry.asInstanceOf[A])
      i += 1
    }
    if (tableDebug) checkConsistent()
  }

  private def checkConsistent() {
    for (i <- 0 until table.length)
      if (table(i) != null && !containsEntry(table(i).asInstanceOf[A]))
        assert(false, i+" "+table(i)+" "+table.mkString)
  }

  protected def elemHashCode(elem: A) = if (elem == null) 0 else elem.hashCode()

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  protected final def index(hcode: Int) = improve(hcode) & (table.length - 1)

  private def newThreshold(size: Int) = {
    val lf = _loadFactor
    assert(lf < (loadFactorDenum / 2), "loadFactor too large; must be < 0.5")
    (size.toLong * lf / loadFactorDenum ).toInt
  }

  protected def clearTable() {
    var i = table.length - 1
    while (i >= 0) { table(i) = null; i -= 1 }
    tableSize = 0
  }
}
