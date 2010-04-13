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

/** A base trait for all sets, mutable as well as immutable.
 *
 * $setNote
 * $setNote2
 * $setTags
 * @since 1.0
 * @author Matthias Zenger
 * @define setNote2
 * '''Implementation note:''' If your additions and mutations return the same kind of set as the set
 *       you are defining, you should inherit from `SetLike` as well.
 */
trait Set[A] extends (A => Boolean)
                with Iterable[A]
                with GenericSetTemplate[A, Set]
                with SetLike[A, Set[A]] {
  override def companion: GenericCompanion[Set] = Set
}

/** $factoryInfo
 *  The current default implementation of a $Coll is one of `EmptySet`, `Set1`, `Set2`, `Set3`, `Set4` in
 *  class `immutable.Set` for sets of sizes up to 4, and a `immutable.HashSet` for sets of larger sizes.
 *  @define coll set
 *  @define Coll Set
 */
object Set extends SetFactory[Set] {
  override def empty[A]: Set[A] = immutable.Set.empty[A]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] = setCanBuildFrom[A]
}
