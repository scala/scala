/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package scala.collection
package parallel.mutable



import scala.collection.mutable.Set
import scala.collection.mutable.Builder








/** A template trait for mutable parallel sets. This trait is mixed in with concrete
 *  parallel sets to override the representation type.
 *
 *  $sideeffects
 *
 *  @tparam T    the element type of the set
 *
 *  @author Aleksandar Prokopec
 *  @since 2.9
 */
trait ParSetLike[T,
                 +Repr <: ParSetLike[T, Repr, Sequential] with ParSet[T],
                 +Sequential <: mutable.Set[T] with mutable.SetLike[T, Sequential]]
extends mutable.SetLike[T, Repr]
   with collection.parallel.ParIterableLike[T, Repr, Sequential]
   with collection.parallel.ParSetLike[T, Repr, Sequential]
{ self =>

  protected[this] override def newBuilder: Builder[T, Repr] = newCombiner

  protected[this] override def newCombiner: parallel.Combiner[T, Repr]

  override def empty: Repr

}








































