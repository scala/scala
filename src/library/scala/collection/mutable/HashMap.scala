/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

/** This class implements mutable maps using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @version 1.0, 08/07/2003
 */
[serializable]
class HashMap[A, B] extends Map[A,B] with HashTable[A] with DefaultMapModel[A,B] {

  def -=(key: A): Unit = removeEntry(key)

  protected def entryKey(e: Entry) = e.key

  override def clear = {
    initTable(table)
    tableSize = 0
  }

  override def clone(): Map[A, B] = {
    val res = new HashMap[A, B]
    res ++= this
    res
  }
}
