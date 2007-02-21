/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import Predef._

/** This class implements mutable maps using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
object HashMap {

  /** The empty map of this type */
  def empty[A, B] = new HashMap[A, B]

  /** The canonical factory for this type
   */
  def apply[A, B](elems: (A, B)*) = empty[A, B] ++ elems
}

@serializable
class HashMap[A, B] extends Map[A,B] with HashTable[A] with DefaultMapModel[A,B] {

  def -= (key: A) { removeEntry(key) }

  override def clear() = super.clear()

  override def clone(): Map[A, B] = new HashMap[A, B] ++ this
}
