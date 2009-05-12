/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scala.collection.generic

/** A generic template for mutable maps from keys of type A to values of type B.
 *  To implement a concrete mutable map, you need to provide implementations of the following methods:
 *
 *   def get(key: A): Option[B]
 *   def elements: Iterator[(A, B)]
 *   def += (kv: (A, B)): this.type
 *   def -= (key: A): this.type
 *
 * If you wish that methods like, take, drop, filter return the same kind of map, you should also
 * override:
 *
 *   def empty: This
 *
 * If you to avoid the unncessary construction of an Option object,
 * you could also override apply, update, and delete.
 * It is also good idea to override methods `foreach` and `size` for efficiency.
 *
 */
trait MutableMapTemplateBase[A, B, +This <: MutableMapTemplateBase[A, B, This] with mutable.Map[A, B]]
  extends MapTemplate[A, B, This] {
  def + [B1 >: B] (kv: (A, B1)): Map[A, B1] = plus(kv)
}
