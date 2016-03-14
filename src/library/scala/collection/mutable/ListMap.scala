/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala
package collection
package mutable

import generic._
import annotation.tailrec

/** A simple mutable map backed by a list, so it preserves insertion order.
 *
 *  @tparam A    the type of the keys contained in this list map.
 *  @tparam B    the type of the values assigned to keys in this list map.
 *
 *  @define Coll `mutable.ListMap`
 *  @define coll mutable list map
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `ListMap[A, B]` if the elements contained in the resulting collection are
 *    pairs of type `(A, B)`. This is because an implicit of type `CanBuildFrom[ListMap, (A, B), ListMap[A, B]]`
 *    is defined in object `ListMap`. Otherwise, `That` resolves to the most specific type that doesn't have
 *    to contain pairs of type `(A, B)`, which is `Iterable`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `ListMap`.
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define orderDependent
 *  @define orderDependentFold
 */
class ListMap[A, B]
extends AbstractMap[A, B]
   with Map[A, B]
   with MapLike[A, B, ListMap[A, B]]
   with Serializable {

  override def empty = ListMap.empty[A, B]

  private var elems: List[(A, B)] = List()
  private var siz: Int = 0

  def get(key: A): Option[B] = elems find (_._1 == key) map (_._2)
  def iterator: Iterator[(A, B)] = elems.iterator

  @deprecatedOverriding("No sensible way to override += as private remove is used in multiple places internally.", "2.11.0")
  def += (kv: (A, B)) = { elems = remove(kv._1, elems, List()); elems = kv :: elems; siz += 1; this }

  @deprecatedOverriding("No sensible way to override -= as private remove is used in multiple places internally.", "2.11.0")
  def -= (key: A) = { elems = remove(key, elems, List()); this }

  @tailrec
  private def remove(key: A, elems: List[(A, B)], acc: List[(A, B)]): List[(A, B)] = {
    if (elems.isEmpty) acc
    else if (elems.head._1 == key) { siz -= 1; acc ::: elems.tail }
    else remove(key, elems.tail, elems.head :: acc)
  }


  @deprecatedOverriding("No sensible way to override as this functionality relies upon access to private methods.", "2.11.0")
  override def clear() = { elems = List(); siz = 0 }

  @deprecatedOverriding("No sensible way to override as this functionality relies upon access to private methods.", "2.11.0")
  override def size: Int = siz
}

/** $factoryInfo
 *  @define Coll `mutable.ListMap`
 *  @define coll mutable list map
 */
object ListMap extends MutableMapFactory[ListMap] {
  implicit def canBuildFrom[A, B]: CanBuildFrom[Coll, (A, B), ListMap[A, B]] = new MapCanBuildFrom[A, B]
  def empty[A, B]: ListMap[A, B] = new ListMap[A, B]
}
