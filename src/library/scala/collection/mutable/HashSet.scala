/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class implements mutable sets using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
[serializable]
class HashSet[A] extends Set[A] with HashTable[A] {

  def contains(elem: A): Boolean = findEntry(elem) match {
    case None => false
    case Some(_) => true
  }

  def +=(elem: A): Unit = findEntry(elem) match {
    case None => addEntry(elem)
    case Some(_) =>
  }

  def -=(elem: A): Unit = removeEntry(elem)

  def elements = entries

  def clear = {
    initTable(table)
    tableSize = 0
  }

  protected type Entry = A

  protected def entryKey(e: Entry) = e

  override def clone(): Set[A] = {
    val res = new HashSet[A]
    res ++= this
    res
  }
}
