/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: HashSet.scala 16893 2009-01-13 13:09:22Z cunei $


package scalax.collection.mutable

import generic.{SetTemplate, SetFactory, AddableBuilder}

/** Factory object for `HashSet` class */
object HashSet extends SetFactory[HashSet] {
  /** The empty set of this type */
  def empty[A] = new HashSet[A]
}

/** This class implements mutable sets using a hashtable.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.0, 31/12/2006
 */
@serializable
class HashSet[A]
  extends Set[A]
     with SetTemplate[HashSet, A]
     with FlatHashTable[A] {

  def contains(elem: A): Boolean = containsEntry(elem)

  def +=(elem: A) { addEntry(elem) }

  def -=(elem: A) { removeEntry(elem) }

  override def clear() = super.clear()

  override def newBuilder[B]: generic.Builder[HashSet, B] =
    new AddableBuilder[HashSet, B](HashSet.empty)

  override def clone(): HashSet[A] = new HashSet[A] ++ this
}


