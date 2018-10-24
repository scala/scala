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

// Methods could be printed by  cat SetLike.scala | egrep '^  (override )?def'

/** This trait implements a proxy for sets. It forwards
 *  all calls to a different set.
 *
 *  @author  Martin Odersky
 *  @since   2.8
 */
@deprecated("proxying is deprecated due to lack of use and compiler-level support", "2.11.0")
trait SetProxyLike[A, +This <: SetLike[A, This] with Set[A]] extends SetLike[A, This] with IterableProxyLike[A, This] {
  def empty: This
  override def contains(elem: A): Boolean = self.contains(elem)
  override def + (elem: A) = self.+(elem)
  override def - (elem: A) = self.-(elem)
  override def isEmpty: Boolean = self.isEmpty
  override def apply(elem: A): Boolean = self.apply(elem)
  override def intersect(that: GenSet[A]) = self.intersect(that)
  override def &(that: GenSet[A]): This = self.&(that)
  override def union(that: GenSet[A]): This = self.union(that)
  override def | (that: GenSet[A]): This = self.|(that)
  override def diff(that: GenSet[A]): This = self.diff(that)
  override def &~(that: GenSet[A]): This = self.&~(that)
  override def subsetOf(that: GenSet[A]): Boolean = self.subsetOf(that)
}
