/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.generic
import scala.collection._

// Methods could be printed by  cat SetTemplate.scala | egrep '^  (override )?def'

/** This trait implements a proxy for sets. It forwards
 *  all calls to a different set.
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait SetProxyTemplate[A, +This <: SetTemplate[A, This] with Set[A]] extends SetTemplate[A, This] with IterableProxyTemplate[A, This]
{
  // def empty: This
  // def + (elem: A): This
  // def - (elem: A): This
  override def contains(elem: A): Boolean = self.contains(elem)
  override def isEmpty: Boolean = self.isEmpty
  override def apply(elem: A): Boolean = self.apply(elem)
  override def intersect(that: Set[A]) = self.intersect(that)
  override def &(that: Set[A]): This = self.&(that)
  override def union(that: Set[A]): This = self.union(that)
  override def | (that: Set[A]): This = self.|(that)
  override def diff(that: Set[A]): This = self.diff(that)
  override def &~(that: Set[A]): This = self.&~(that)
  override def subsetOf(that: Set[A]): Boolean = self.subsetOf(that)
}
