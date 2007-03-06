/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.immutable

object HashSet {

  /** The empty set of this type.
   */
  def empty[A] = new HashSet[A]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: A*) = empty[A] ++ elems
}

@serializable
class HashSet[A] extends Set[A] with mutable.FlatHashTable[A] {
  protected var later: HashSet[A] = null
  protected var changedElem: A = _
  protected var deleted: Boolean = _

  def empty[C]: Set[C] = new EmptySet[C]

  def contains(elem: A): Boolean = {
    var m = this
    var cnt = 0
    while (m.later != null) {
      if (elem == m.changedElem) return m.deleted
      cnt = cnt + 1
      m = m.later
    }
    if (cnt > logLimit) makeCopy(m)
    m.containsEntry(elem)
  }

  def + (elem: A): Set[A] = {
    makeCopyIfUpdated()
    if (containsEntry(elem)) this
    else {
      markUpdated(elem, false)
      later addEntry elem
      later
    }
  }

  def - (elem: A): Set[A] = {
    makeCopyIfUpdated()
    if (!containsEntry(elem)) this
    else {
      markUpdated(elem, true)
      later removeEntry elem
      later
    }
  }

  override def size: Int = {
    var m = this
    var cnt = 0
    var s = tableSize
    while (m.later != null) {
      if (m.deleted) s = s + 1 else s = s - 1
      cnt = cnt + 1
      m = m.later
    }
    if (cnt > logLimit) makeCopy(m)
    s
  }

  override def elements = {
    makeCopyIfUpdated()
    super.elements
  }

  private def logLimit: Int = Math.sqrt(table.length.toDouble).toInt

  private def markUpdated(elem: A, del: Boolean) {
    val lv = loadFactor
    later = new HashSet[A] {
      override def initialSize = 0
      override def loadFactor = lv
      table     = HashSet.this.table
      tableSize = HashSet.this.tableSize
      threshold = HashSet.this.threshold
    }
    changedElem = elem
    deleted = del
  }

  private def makeCopy(last: HashSet[A]) {
    def undo(m: HashSet[A]) {
      if (m ne last) {
        undo(m.later)
        if (m.deleted) addEntry(m.changedElem)
        else removeEntry(m.changedElem)
      }
    }
    table = new Array[AnyRef](last.table.length)
    Array.copy(last.table, 0, table, 0, table.length)
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

