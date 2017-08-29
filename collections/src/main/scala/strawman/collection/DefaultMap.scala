/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package strawman
package collection

/** A default map which builds a default `immutable.Map` implementation for all
  * transformations.
  *
  *  Instances that inherit from `DefaultMap[K, V]` still have to define:
  *  {{{
  *    def get(key: K): Option[V]
  *    def iterator(): Iterator[(K, V)]
  *  }}}
  *
  *  It might also be advisable to override `foreach` or `size` if efficient
  *  implementations can be found.
  *
  *  @since 2.8
  */
trait DefaultMap[K, +V] extends Map[K, V] { self =>

  // Members declared in IterableOps
  def iterableFactory: IterableFactory[Iterable] = Iterable
  protected[this] def fromSpecificIterable(coll: Iterable[(K, V)]): Map[K,V] = mapFactory.from(coll)
  protected[this] def newSpecificBuilder(): mutable.Builder[(K, V), Map[K,V]] = mapFactory.newBuilder()

  // Members declared in MapOps
  def mapFactory: MapFactory[Map] = Map
  def empty: Map[K,V] = mapFactory.empty
  protected[this] def mapFromIterable[K2, V2](it: Iterable[(K2, V2)]): Map[K2,V2] = mapFactory.from(it)
}
