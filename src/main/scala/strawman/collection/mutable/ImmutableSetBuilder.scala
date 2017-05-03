package strawman
package collection
package mutable

/** The canonical builder for immutable Sets.
  *
  *  @tparam A      The type of the elements that will be contained in this set.
  *  @tparam C      The type of the actual collection this set builds.
  *  @param empty   The empty element of the collection.
  *  @since 2.13
  */
class ImmutableSetBuilder[A, CC[X] <: immutable.Set[X] with immutable.SetOps[X, CC[X]]](empty: CC[A])
  extends ImmutableBuilder[A, CC[A]](empty) {

  def add(x: A): this.type = { elems = elems + x; this }
}
