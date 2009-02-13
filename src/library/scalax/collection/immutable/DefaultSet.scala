/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: HashSet.scala 16884 2009-01-09 16:52:09Z cunei $

package scalax.collection.immutable

import generic.SetTemplate

/** A default implementation of immutable sets.
 *  This is currently implemented as a proxy for an immutable HashSet,
 *  except that its builder returns specialized representations EmptySet,Set1,..., Set4
 *  for sets of size <= 4.
 */
class DefaultSet[A] private (hset: HashSet[A])
  extends Set[A]
     with SetTemplate[Set, A] {

  def this() = this(new HashSet[A])

  def contains(elem: A): Boolean = hset.contains(elem)

  def + (elem: A): Set[A] = hset + elem

  /** Keeps underlying HashSet representation, but switches back to EmptySet if
   *  result does not contain any elements
   */
  def - (elem: A): Set[A] = {
    val hset1 = hset - elem
    if (hset1.isEmpty) new EmptySet[A]
    else new DefaultSet(hset)
  }

  def size: Int = hset.size

  def elements: Iterator[A] = hset.elements

  override def foreach(f: A => Unit): Unit = hset.foreach(f)
}

