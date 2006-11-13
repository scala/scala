/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.util

class HashSet[T >: Null <: AnyRef](initialCapacity: int) extends Set[T] {

  def this() = this(16)

  private var capacity = initialCapacity
  private var used = 0
  private var table = new Array[AnyRef](capacity)

  def size: int = used

  def findEntry(x: T): T = {
    var h = x.hashCode() % capacity
    var entry = table(h)
    while ((entry ne null) && entry != x) {
      h = (h + 1) % capacity
      entry = table(h)
    }
    entry.asInstanceOf[T]
  }

  def addEntry(x: T): unit = {
    var h = x.hashCode() % capacity
    var entry = table(h)
    while (entry ne null) {
      if (entry == x) return
      h = (h + 1) % capacity
      entry = table(h)
    }
    table(h) = x
    used = used + 1
    if (used >= (capacity >> 2)) growTable
  }

  def elements = new Iterator[T] {
    private var i = 0
    def hasNext: boolean = {
      while (i < capacity && (table(i) eq null)) i = i + 1;
      i < capacity
    }
    def next: T =
      if (hasNext) { i = i + 1; table(i - 1).asInstanceOf[T] }
      else null
  }

  private def growTable: unit = {
    val oldtable = table
    capacity = capacity * 2
    table = new Array[AnyRef](capacity)
    var i = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (entry ne null) addEntry(entry.asInstanceOf[T])
      i = i + 1
    }
  }
}
