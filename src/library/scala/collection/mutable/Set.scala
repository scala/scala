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

/** This class represents mutable sets. Concrete set implementations
 *  have to provide functionality for the abstract methods in Set:
 *
 *  def contains(elem: A): Boolean
 *  def put(elem: A): Boolean
 *  def remove(elem: A): Boolean
 *  def elements: Iterator[A]
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 */
trait Set[A] extends Iterable[A]
                with collection.Set[A]
                with MutableSetTemplate[A, Set[A]]
                with Unhashable {

  override def empty = Set.empty

  override def traversableBuilder[B]: Builder[B, Set[B]] = Set.newBuilder[B]
}

/** The canonical factory methods for <a href="Set.html">mutable sets</a>.
 *  Currently this returns a HashSet.
 */
object Set extends SetFactory[Set] {
  type Coll = Set[_]
  implicit def builderFactory[A]: BuilderFactory[A, Set[A], Coll] = new BuilderFactory[A, Set[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
  def empty[A]: Set[A] = HashSet.empty[A]
}

