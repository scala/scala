/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$



package scala.collection.immutable


/** An immutable tree-based set that uses hashes to order
 * the elements.  While it is not as efficient as
 * a regular TreeSet, it has the advantage of not requiring
 * an Ordered view being available for the data.
 *
 * The implementation is a TreeSet mapping hash codes into
 * ListSet's, where each ListSet is a bucket of entries
 * whose elements all have the same hash code.
 */
class HashTreeSet[T](unitmap: Map[T,Unit])
extends Set[T]
{
  /** Create an empty HashTreeMap */
  def this() = this(new HashTreeMap)

  def size = unitmap.size

  def +(x: T) = new HashTreeSet(unitmap + {x,()})

  def -(x: T) = new HashTreeSet(unitmap - x)

  def contains(x: T) = unitmap.contains(x)

  def empty[S]: Set[S] = new HashTreeSet[S]

  def elements = new scala.Iterator[T] {
    val mapElements = unitmap.elements
    def hasNext = mapElements.hasNext
    def next = mapElements.next._1
  }
}