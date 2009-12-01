/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** This class implements mutable sets using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 *  @since   1
 */
@serializable @SerialVersionUID(1L)
class HashSet[A] extends Set[A]
                    with GenericSetTemplate[A, HashSet]
                    with SetLike[A, HashSet[A]]
                    with FlatHashTable[A] {
  override def companion: GenericCompanion[HashSet] = HashSet

  override def size = tableSize

  def contains(elem: A): Boolean = containsEntry(elem)

  def += (elem: A): this.type = { addEntry(elem); this }
  def -= (elem: A): this.type = { removeEntry(elem); this }

  override def add(elem: A): Boolean = addEntry(elem)
  override def remove(elem: A): Boolean = removeEntry(elem).isDefined

  override def clear() = clearTable()

  override def foreach[U](f: A =>  U) {
    var i = 0
    val len = table.length
    while (i < len) {
      val elem = table(i)
      if (elem ne null) f(elem.asInstanceOf[A])
      i += 1
    }
  }

  override def clone(): Set[A] = new HashSet[A] ++= this

  private def writeObject(s: java.io.ObjectOutputStream) {
    serializeTo(s)
  }

  private def readObject(in: java.io.ObjectInputStream) {
    init(in, x => x)
  }
}

/** Factory object for `HashSet` class */
object HashSet extends SetFactory[HashSet] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, HashSet[A]] = setCanBuildFrom[A]
  override def empty[A]: HashSet[A] = new HashSet[A]
}

