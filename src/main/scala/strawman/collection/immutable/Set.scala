package strawman
package collection.immutable

import strawman.collection.mutable.Builder
import strawman.collection.IterableFactory

import scala.Any

/** Base trait for immutable set collections */
trait Set[A]
  extends collection.Set[A]
    with Iterable[A]
    with SetLike[A, Set]

/** Base trait for immutable set operations */
trait SetLike[A, +C[X] <: Set[X] with SetLike[X, C]]
  extends collection.SetLike[A, C]
    with SetMonoTransforms[A, C[A]]
    with SetPolyTransforms[A, C]

/** Transformation operations returning a Set containing the same kind of
  * elements
  */
trait SetMonoTransforms[A, +Repr]
  extends collection.SetMonoTransforms[A, Repr] {

  def + (elem: A): Repr

  def - (elem: A): Repr

}

trait SetPolyTransforms[A, +C[X] <: Set[X] with SetLike[X, C]]
  extends collection.SetPolyTransforms[A, C] {

  protected def coll: C[A]

  def ++ (that: collection.Set[A]): C[A] = {
    var result: C[A] = coll
    val it = that.iterator()
    while (it.hasNext) result = result + it.next()
    result
  }

}

object Set extends IterableFactory[Set] {
  def empty[A <: Any]: Set[A] = HashSet.empty
  def newBuilder[A <: Any]: Builder[A, Set[A]] = HashSet.newBuilder
  def fromIterable[E](it: strawman.collection.Iterable[E]): Set[E] = HashSet.fromIterable(it)
}