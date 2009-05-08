/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection

import generic._

/** <p>
 *    A set is a collection that includes at most one of any object.
 *  </p>
 *  <p>
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait Set[A] extends (A => Boolean) with Iterable[A] with SetTemplate[A, Set[A]] {
  def empty = Set.empty
  override def traversableBuilder[B]: Builder[B, Set[B], Any] = Set.newBuilder[B]
}

/* Factory object for `Set` class */
object Set extends SetFactory[Set] {
  def empty[A]: Set[A] = immutable.Set.empty[A]
  type Coll = Set[_]
  implicit def builderFactory[A]: BuilderFactory[A, Set[A], Coll] = new BuilderFactory[A, Set[A], Coll] { def apply(from: Coll) = from.traversableBuilder[A] }
}

/* !!! what to do about this?
override def hashCode() =
    (0 /: this)((hash, e) => hash + e.hashCode())

  override def toArray[B >: A]: Array[B] = {
    val result = new Array[B](size)
    copyToArray(result, 0)
    result
  }

  /** Defines the prefix of this object's <code>toString</code> representation.
   */
  override protected def stringPrefix : String = "Set"
*/
