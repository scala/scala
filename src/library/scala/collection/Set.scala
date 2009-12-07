/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
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
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 */
trait Set[A] extends (A => Boolean)
                with Iterable[A]
                with GenericSetTemplate[A, Set]
                with SetLike[A, Set[A]] {
  override def companion: GenericCompanion[Set] = Set
}

/** Factory object for <code>Set</code> class.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
object Set extends SetFactory[Set] {
  override def empty[A]: Set[A] = immutable.Set.empty[A]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] = setCanBuildFrom[A]
}
