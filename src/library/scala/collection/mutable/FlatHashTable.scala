/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: HashSet.scala 9235 2006-11-13 14:59:18 +0000 (Mon, 13 Nov 2006) mihaylov $

package scala.collection.mutable

trait FlatHashTable[A] {

  /** The load factor for the hash table.
   */
  protected def loadFactor: Float = 0.5f

  /** The initial size of the hash table.
   */
  protected def initialSize: Int = 16

  /** The actual hash table.
   */
  protected var table: Array[AnyRef] =
    if (initialSize == 0) null else new Array(initialSize)

  /** The number of mappings contained in this hash table.
   */
  protected var tableSize = 0

  /** The next size value at which to resize (capacity * load factor).
   */
  protected var threshold: Int = newThreshold(initialSize)

  /** Returns the number of entires in this hash table.
   */
  def size: Int = tableSize

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

  def addEntry(elem: A) {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry) {
      if (entry == elem) return
      h = (h + 1) % table.length
      entry = table(h)
    }
    table(h) = elem.asInstanceOf[AnyRef]
    tableSize = tableSize + 1
    if (tableSize >= threshold) growTable()
  }

  def removeEntry(elem: A) {
    var h = index(elemHashCode(elem))
    var entry = table(h)
    while (null != entry) {
      if (entry == elem) {
        var h1 = (h + 1) % table.length
        while (null != table(h1) && (index(elemHashCode(table(h1).asInstanceOf[A])) != h1)) {
          table(h) = table(h1)
          h = h1
          h1 = (h + 1) % table.length
        }
        table(h) = null
        tableSize = tableSize - 1
        return
      }
      h = (h + 1) % table.length
      entry = table(h)
    }
  }

  def elements = new Iterator[A] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < table.length && (null == table(i))) i = i + 1;
      i < table.length
    }
    def next: A =
      if (hasNext) { i = i + 1; table(i - 1).asInstanceOf[A] }
      else Iterator.empty.next
  }

  private def growTable() {
    val oldtable = table
    table = new Array[AnyRef](table.length * 2)
    threshold = newThreshold(table.length)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (null != entry) addEntry(entry.asInstanceOf[A])
      i = i + 1
    }
  }

  protected def elemHashCode(elem: A) = elem.hashCode()

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  protected final def index(hcode: Int) = improve(hcode) & (table.length - 1)

  private def newThreshold(size: Int) =
    (size * loadFactor).asInstanceOf[Int]

  protected def clear() {
    var i = table.length - 1
    while (i >= 0) { table(i) = null; i = i - 1 }
    tableSize = 0
  }
}
