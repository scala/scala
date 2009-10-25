/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package util

class HashSet[T >: Null <: AnyRef](val label: String, initialCapacity: Int) extends Set[T] {
  def this(initialCapacity: Int) = this("No Label", initialCapacity)
  def this(label: String) = this(label, 16)
  def this() = this(16)

  private var capacity = initialCapacity
  private var used = 0
  private var table = new Array[AnyRef](capacity)
  // System.err.println("Created: " + this)

  def size: Int = used
  def clear() {
    capacity = initialCapacity
    used = 0
    table = new Array[AnyRef](capacity)
  }

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
    if (used > (capacity >> 2)) growTable()
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
    val growthFactor =
      if (capacity <= initialCapacity) 8
      else if (capacity <= (initialCapacity * 8)) 4
      else 2

    capacity *= growthFactor
    table = new Array[AnyRef](capacity)
    var i = 0
    used = 0
    while (i < oldtable.length) {
      val entry = oldtable(i)
      if (entry ne null) addEntry(entry.asInstanceOf[T])
      i += 1
    }
    // System.err.println("Grown: " + this)
  }
  override def toString() = "HashSet %s(%d / %d)".format(label, used, capacity)
}
