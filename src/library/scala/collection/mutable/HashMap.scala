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
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
[serializable]
class HashMap[A, B] extends Map[A,B] with HashTable[A] with DefaultMapModel[A,B] {

  def -= (key: A) { removeEntry(key) }

  override def clear = {
    initTable()
    tableSize = 0
  }

  override def clone(): Map[A, B] = new HashMap[A, B] ++ this
}
