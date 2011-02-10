/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection.parallel




import scala.collection.MapLike
import scala.collection.Map
import scala.collection.mutable.Builder








/** A template trait for mutable parallel maps. This trait is to be mixed in
 *  with concrete parallel maps to override the representation type.
 *
 *  $sideeffects
 *
 *  @tparam K    the key type of the map
 *  @tparam V    the value type of the map
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParMapLike[K,
                 +V,
                 +Repr <: ParMapLike[K, V, Repr, Sequential] with ParMap[K, V],
                 +Sequential <: Map[K, V] with MapLike[K, V, Sequential]]
extends MapLike[K, V, Repr]
   with ParIterableLike[(K, V), Repr, Sequential]
{ self =>

  protected[this] override def newBuilder: Builder[(K, V), Repr] = newCombiner

  protected[this] override def newCombiner: Combiner[(K, V), Repr] = unsupportedop("Must implement `newCombiner` in concrete collections.")

  override def empty: Repr

  private type T = (K, V)
  override def toParMap[K, V](implicit ev: T <:< (K, V)) = this.asInstanceOf[ParMap[K, V]]

}












