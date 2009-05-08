/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.mutable

import Predef._

trait FlatHashTable[A] {

  /** The load factor for the hash table; must be < 500 (0.5)
   */
  protected def loadFactor: Int = 450
  protected final def loadFactorDenum = 1000

  /** The initial size of the hash table.
   */
  protected def initialSize: Int = 16

  private final val tableDebug = false

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

  def elements = new Iterator[A] {
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
        assert(false, i+" "+table(i)+" "+table.toString)
  }

  protected def elemHashCode(elem: A) = elem.hashCode()

  protected final def improve(hcode: Int) = {
    var h: Int = hcode + ~(hcode << 9)
    h = h ^ (h >>> 14)
    h = h + (h << 4)
    h ^ (h >>> 10)
  }

  protected final def index(hcode: Int) = improve(hcode) & (table.length - 1)

  private def newThreshold(size: Int) = {
    val lf = loadFactor
    assert(lf < (loadFactorDenum / 2), "loadFactor too large; must be < 0.5")
    (size.toLong * lf / loadFactorDenum ).toInt
  }

  protected def clear() {
    var i = table.length - 1
    while (i >= 0) { table(i) = null; i -= 1 }
    tableSize = 0
  }
}
