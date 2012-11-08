/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.collection
package mutable

import generic._

/** A simple mutable map backed by a list.
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
  def += (kv: (A, B)) = { elems = remove(kv._1, elems); elems = kv :: elems; siz += 1; this }
  def -= (key: A) = { elems = remove(key, elems); this }

  private def remove(key: A, elems: List[(A, B)]): List[(A, B)] =
    if (elems.isEmpty) elems
    else if (elems.head._1 == key) { siz -= 1; elems.tail }
    else elems.head :: remove(key, elems.tail)

  override def clear() = { elems = List(); siz = 0 }
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
