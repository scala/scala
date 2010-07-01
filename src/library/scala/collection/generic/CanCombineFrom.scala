package scala.collection
package generic

import scala.collection.parallel._

/**
 * A base trait for parallel builder factories.
 *
 * @tparam From   the type of the underlying collection that requests a builder to be created
 * @tparam Elem   the element type of the collection to be created
 * @tparam To     the type of the collection to be created
 */
trait CanCombineFrom[-From, -Elem, +To] extends CanBuildFrom[From, Elem, To] with Parallel {
  def apply(from: From): Combiner[Elem, To]
  def apply(): Combiner[Elem, To]
}







