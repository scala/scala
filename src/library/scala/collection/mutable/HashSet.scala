/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection.mutable

import generic._

/** This class implements mutable sets using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
@serializable
class HashSet[A] extends Set[A] with MutableSetTemplate[A, HashSet[A]] with FlatHashTable[A] {

  override def empty = HashSet.empty
  override def traversableBuilder[B]: Builder[B, HashSet[B]] = HashSet.newBuilder[B]

  override def size = super.size

  def contains(elem: A): Boolean = containsEntry(elem)

  def put(elem: A): Boolean = addEntry(elem)

  def remove(elem: A): Boolean = !removeEntry(elem).isEmpty

  override def clear() = super.clear()

  override def foreach[U](f: A =>  U) {
    var i = 0
    val len = table.length
    while (i < len) {
      val elem = table(i)
      if (elem ne null) f(elem.asInstanceOf[A])
      i += 1
    }
  }

  override def clone(): HashSet[A] = new HashSet[A] ++ this
}

/** Factory object for `HashSet` class */
object HashSet extends SetFactory[HashSet] {
  type Coll = HashSet[_]
  implicit def builderFactory[A]: BuilderFactory[A, HashSet[A], Coll] = new BuilderFactory[A, HashSet[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def empty[A]: HashSet[A] = new HashSet[A]
}

