/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.immutable

import Predef._

/** This class implements immutable maps using a hashtable.
 *
 *  @author  Martin Odersky
 *  @version 2.0, 19/01/2007
 */
object HashMap {

  /** The empty map of this type */
  def empty[A, B] = new HashMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: (A, B)*) = empty[A, B] ++ elems
}

@serializable
class HashMap[A, B] extends Map[A,B] with mutable.HashTable[A] {
  type Entry = mutable.DefaultEntry[A, Any]

  protected var later: HashMap[A, B] = null
  protected var oldKey: A = _
  protected var oldValue: Option[B] = _
  protected var deltaSize: int = _

  def empty[C]: Map[A, C] = new EmptyMap[A, C]

  def get(key: A): Option[B] = {
    var m = this
    var cnt = 0
    while (m.later != null) {
      if (key == m.oldKey) return m.oldValue
      cnt = cnt + 1
      m = m.later
    }
    if (cnt > logLimit) makeCopy(m)
    val e = m.findEntry(key)
    if (e == null) None
    else Some(getValue(e))
  }

  def update [B1 >: B](key: A, value: B1): Map[A, B1] = {
    makeCopyIfUpdated()
    val e = findEntry(key)
    if (e == null) {
      markUpdated(key, None, 1)
      later.addEntry(new Entry(key, value))
    } else {
      markUpdated(key, Some(getValue(e)), 0)
      e.value = value
    }
    later
  }

  def - (key: A): Map[A, B] = {
    makeCopyIfUpdated()
    val e = findEntry(key)
    if (e == null) this
    else {
      markUpdated(key, Some(getValue(e)), -1)
      later removeEntry key
      later
    }
  }

  override def size: int = {
    var m = this
    var cnt = 0
    var s = tableSize
    while (m.later != null) {
      s = s - m.deltaSize
      cnt = cnt + 1
      m = m.later
    }
    if (cnt > logLimit) makeCopy(m)
    s
  }

  def elements = {
    makeCopyIfUpdated()
    entries map {e => (e.key, getValue(e))}
  }

  private def getValue(e: Entry) =
    e.value.asInstanceOf[B]

  private def logLimit: int = Math.sqrt(table.length.toDouble).toInt

  private def markUpdated(key: A, ov: Option[B], delta: int) {
    val lv = loadFactor
    later = new HashMap[A, B] {
      override def initialSize = 0
      override def loadFactor = lv
      table     = HashMap.this.table
      tableSize = HashMap.this.tableSize
      threshold = HashMap.this.threshold
    }
    oldKey = key
    oldValue = ov
    deltaSize = delta
  }

  private def makeCopy(last: HashMap[A, B]) {
    def undo(m: HashMap[A, B]) {
      if (m ne last) {
        undo(m.later)
        if (m.deltaSize == 1) removeEntry(m.oldKey)
        else if (m.deltaSize == 0) findEntry(m.oldKey).value = m.oldValue.get
        else if (m.deltaSize == -1) addEntry(new Entry(m.oldKey, m.oldValue.get))
      }
    }
    def copy(e: Entry): Entry =
      if (e == null) null
      else {
        val rest = copy(e.next)
        val result = new Entry(e.key, e.value)
        result.next = rest
        result
      }
    val ltable = last.table
    val s = ltable.length
    table = new Array[Entry](s)
    var i = 0
    while (i < s) {
      table(i) = copy(ltable(i))
      i = i + 1
    }
    tableSize = last.tableSize
    threshold = last.threshold
    undo(this)
    later = null
  }

  private def makeCopyIfUpdated() {
    var m = this
    while (m.later != null) m = m.later
    if (m ne this) makeCopy(m)
  }
}

