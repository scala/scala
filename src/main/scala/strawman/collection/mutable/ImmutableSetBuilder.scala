package strawman
package collection.mutable

/** The canonical builder for immutable Sets.
  *
  *  @tparam A      The type of the elements that will be contained in this set.
  *  @tparam C      The type of the actual collection this set builds.
  *  @param empty   The empty element of the collection.
  *  @since 2.13
  */
class ImmutableSetBuilder[
  A,
  C[X] <: collection.immutable.Set[X] with collection.immutable.SetMonoTransforms[X, C[X]]
](empty: C[A])
  extends ImmutableBuilder[A, C[A]](empty) {

  def addInPlace(x: A): this.type = { elems = elems + x; this }

}
