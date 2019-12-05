/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package collection

import generic._

/** A base trait for all sets, mutable as well as immutable.
 *
 * $setNote
 * '''Implementation note:''' If your additions and mutations return the same kind of set as the set
 *       you are defining, you should inherit from `SetLike` as well.
 * $setTags
 *
 * @since 1.0
 * @author Matthias Zenger
 */
trait Set[A] extends (A => Boolean)
                with Iterable[A]
                with GenSet[A]
                with GenericSetTemplate[A, Set]
                with SetLike[A, Set[A]] {
  override def companion: GenericCompanion[Set] = Set

  override def seq: Set[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is one of `EmptySet`, `Set1`, `Set2`, `Set3`, `Set4` in
 *  class `immutable.Set` for sets of sizes up to 4, and a `immutable.HashSet` for sets of larger sizes.
 *  @define coll set
 *  @define Coll `Set`
 */
object Set extends SetFactory[Set] {
  def newBuilder[A] = immutable.Set.newBuilder[A]
  override def empty[A]: Set[A] = immutable.Set.empty[A]
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, A, Set[A]]]
  private[this] val ReusableCBF = setCanBuildFrom[Any]
}

/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses. */
abstract class AbstractSet[A] extends AbstractIterable[A] with Set[A]
