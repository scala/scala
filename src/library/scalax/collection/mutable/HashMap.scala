/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: HashMap.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.mutable

import generic._

/** This class implements mutable maps using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
object HashMap  extends MapFactory[HashMap] {

  /** The empty map of this type */
  def empty[A, B] = new HashMap[A, B]

}

@serializable
class HashMap[A, B]
  extends Map[A, B]
     with MapTemplate[A, B, HashMap]
     with HashTable[A]
     with DefaultMapModel[A, B] {

  def empty[B] = HashMap.empty[A, B]

  def -= (key: A) { removeEntry(key) }

  override def clear() = super.clear()

  override def clone(): Map[A, B] = new HashMap[A, B] ++ this
}
