/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package util

class HashSet[T >: Null <: AnyRef](initialCapacity: Int) extends Set[T] {

  def this() = this(16)

  private var capacity = initialCapacity
  private var used = 0
  private var table = new Array[AnyRef](capacity)

  def size: Int = used

  private def index(x: Int): Int = Math.abs(x % capacity)

  def findEntry(x: T): T = {
    var h = index(x.hashCode())
    var entry = table(h)
    while ((entry ne null) && entry != x) {
      h = index(h + 1)
      entry = table(h)
    }
    entry.asInstanceOf[T]
  }

  def addEntry(x: T) {
    var h = index(x.hashCode())
    var entry = table(h)
    while (entry ne null) {
      if (entry == x) return
      h = index((h + 1))
      entry = table(h)
    }
    table(h) = x
    used += 1
    if (used >= (capacity >> 2)) growTable()
  }

  def iterator = new Iterator[T] {
    private var i = 0
    def hasNext: Boolean = {
      while (i < capacity && (table(i) eq null)) i += 1
      i < capacity
    }
    def next: T =
      if (hasNext) { i += 1; table(i - 1).asInstanceOf[T] }
      else null
  }

  private def growTable() {
    val oldtable = table
    capacity *= 2
    table = new Array[AnyRef](capacity)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (entry ne null) addEntry(entry.asInstanceOf[T])
      i += 1
    }
  }
}
