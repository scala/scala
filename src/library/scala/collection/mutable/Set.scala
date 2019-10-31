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
package mutable

import generic._

/** A generic trait for mutable sets.
 *  $setNote
 *  $setTags
 *
 *  @since 1.0
 *  @author Matthias Zenger
 *  @define Coll `mutable.Set`
 *  @define coll mutable set
 */
trait Set[A] extends Iterable[A]
//                with GenSet[A]
                with scala.collection.Set[A]
                with GenericSetTemplate[A, Set]
                with SetLike[A, Set[A]] {
  override def companion: GenericCompanion[Set] = Set
  override def seq: Set[A] = this
}

/** $factoryInfo
 *  The current default implementation of a $Coll is a `HashSet`.
 *  @define coll mutable set
 *  @define Coll `mutable.Set`
 */
object Set extends MutableSetFactory[Set] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] =
    ReusableCBF.asInstanceOf[CanBuildFrom[Coll, A, Set[A]]]
  private[this] val ReusableCBF = setCanBuildFrom[Any]
  override def empty[A]: Set[A] = HashSet.empty[A]
}

/** Explicit instantiation of the `Set` trait to reduce class file size in subclasses. */
abstract class AbstractSet[A] extends AbstractIterable[A] with Set[A]
