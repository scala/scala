/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.util;

class HashSet[T <: AnyRef](capacity: int) {

  private var size = capacity;
  private var used = 0;
  private var table = new Array[Object](size);

  def findEntry(x: T): T = {
    var h = x.hashCode() % size;
    var entry = table(h);
    while (entry != null && entry != x) {
      h = (h + 1) % size;
      entry = table(h)
    }
    entry.asInstanceOf[T]
  }

  def addEntry(x: T): unit = {
    if (used >= (size >> 2)) growTable;
    used = used + 1;
    var h = x.hashCode() % size;
    while (table(h) != null) {
      h = (h + 1) % size
    }
    table(h) = x
  }

  def elements = new Iterator[T] {
    private var i = 0;
    def hasNext: boolean = {
      while (i < size && table(i) == null) i = i + 1;
      i < size
    }
    def next: T =
      if (hasNext) { i = i + 1; table(i - 1).asInstanceOf[T] }
      else null
  }

  private def growTable: unit = {
    val oldtable = table;
    size = size * 2;
    table = new Array[Object](size);
    var i = 0;
    while (i < oldtable.length) {
      val entry = oldtable(i);
      if (entry != null) addEntry(entry.asInstanceOf[T]);
      i = i + 1
    }
  }
}
