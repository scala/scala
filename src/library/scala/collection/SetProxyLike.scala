/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala
package collection

// Methods could be printed by  cat SetLike.scala | egrep '^  (override )?def'

/** This trait implements a proxy for sets. It forwards
 *  all calls to a different set.
 *
 *  @author  Martin Odersky
 *  @version 2.8
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
