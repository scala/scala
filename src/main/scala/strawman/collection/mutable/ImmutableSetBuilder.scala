package strawman
package collection.mutable

import scala.Unit

/** The canonical builder for immutable Sets.
  *
  *  @tparam A      The type of the elements that will be contained in this set.
  *  @tparam C      The type of the actual collection this set builds.
  *  @param empty   The empty element of the collection.
  *  @since 2.13
  */
class ImmutableSetBuilder[
  A,
  C[X] <: strawman.collection.immutable.Set[X] with strawman.collection.immutable.SetMonoTransforms[X, C[X]]
](empty: C[A])
  extends ReusableBuilder[A, C[A]] {

  type Coll = C[A]
  protected var elems: Coll = empty
  def +=(x: A): this.type = { elems = elems + x; this }
  def clear(): Unit = { elems = empty }
  def result(): Coll = elems
}
