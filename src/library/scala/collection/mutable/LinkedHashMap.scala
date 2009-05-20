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
import generic._

/** This class implements mutable maps using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
object LinkedHashMap extends MutableMapFactory[LinkedHashMap] {
  implicit def builderFactory[A, B]: BuilderFactory[(A, B), LinkedHashMap[A, B], Coll] = new MapBuilderFactory[A, B]
  def empty[A, B] = new LinkedHashMap[A, B]
}

@serializable
class LinkedHashMap[A, B] extends Map[A, B]
                             with MutableMapTemplate[A, B, LinkedHashMap[A, B]]
                             with HashTable[A]
                             with DefaultMapModel[A,B] {

  override def empty = LinkedHashMap.empty

  override def size = super[HashTable].size

  private var ordered = List[Entry]()

  def -= (key: A): this.type = { remove(key); this }

  override def remove(key: A): Option[B] = removeEntry(key) match {
    case None => None
    case Some(e) =>
      ordered = ordered.filter(_ ne e)
      Some(e.value)
    }

  override def put(key: A, value: B): Option[B] = {
    val e = findEntry(key)
    if (e == null) {
      val e = new Entry(key, value)
      ordered = e :: ordered
      addEntry(e)
      None
    } else {
      val ret = Some(e.value)
      e.value = value
      ret
    }
  }
  override def update(key: A, value: B) { put(key, value) }

  override def clear() {
    ordered = Nil
    super.clear()
  }

  override def elements = ordered.reverse.elements map {e => (e.key, e.value)}

  // debug NoSuchElementException in Pickler
  var previousTables: List[(String, Int)] = Nil

  override protected def resize(newSize: Int) {
    previousTables = (tableString(table), table.length) :: previousTables
    super.resize(newSize)
    // check consistency
    for (i <- 0 until table.length) {
      var e = table(i)
      while (e != null) {
        if (index(e.key.hashCode()) != i) {
          println("INCONSISTENT entry at "+ i +" after resize. before:")
          println("index before: "+ indx(e.key.hashCode(), previousTables.head._2))
          println(previousTables.head._1)
          println("AFTER:")
          println("index after: "+ index(e.key.hashCode()))
          println(tableString(table))
        }
        e = e.next
      }
    }
  }

  def tableString(table: Array[HashEntry[A, Entry]]): String = {
    val sb = new StringBuilder
    for (i <- 0 until table.length) {
      sb.append(""+ i +": ")
      var e = table(i).asInstanceOf[Entry]
      while (e != null) {
        sb.append("("+ (try { e.key.toString } catch { case _ => "<..>"}) +" -> "+ e.value +"), ")
        e = e.next
      }
      sb.append("\n")
    }
    sb.toString
  }

  def indx(hash: Int, length: Int) = improve(hash) & (length - 1)
  def printHashTable(notFoundHash: Int) {
    println("Hash not found: "+ notFoundHash)
    println("hashtables")
    for (tb <- previousTables.reverse) {
      println("with index: "+ indx(notFoundHash, tb._2))
      println(tb._1)
      println(" ==> RESIZE, then:")
    }
    println("with index: "+ index(notFoundHash))
    println(tableString(table))
  }
}
