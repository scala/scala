/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.util;

class HashSet[T <: AnyRef](initialCapacity: int) extends Set[T] {

  private var capacity = initialCapacity;
  private var used = 0;
  private var table = new Array[Object](capacity);

  def size: int = used;

  def findEntry(x: T): T = {
    var h = x.hashCode() % capacity;
    var entry = table(h);
    while (entry != null && entry != x) {
      h = (h + 1) % capacity;
      entry = table(h)
    }
    entry.asInstanceOf[T]
  }

  def addEntry(x: T): unit = {
    if (used >= (capacity >> 2)) growTable;
    used = used + 1;
    var h = x.hashCode() % capacity;
    while (table(h) != null) {
      h = (h + 1) % capacity
    }
    table(h) = x
  }

  def elements = new Iterator[T] {
    private var i = 0;
    def hasNext: boolean = {
      while (i < capacity && table(i) == null) i = i + 1;
      i < capacity
    }
    def next: T =
      if (hasNext) { i = i + 1; table(i - 1).asInstanceOf[T] }
      else null
  }

  private def growTable: unit = {
    val oldtable = table;
    capacity = capacity * 2;
    table = new Array[Object](capacity);
    var i = 0;
    while (i < oldtable.length) {
      val entry = oldtable(i);
      if (entry != null) addEntry(entry.asInstanceOf[T]);
      i = i + 1
    }
  }
}
