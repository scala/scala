package strawman.collection.immutable

/** Base trait for immutable set collections */
trait Set[A]
  extends strawman.collection.Set[A]
    with Iterable[A]
    with SetLike[A, Set]

/** Base trait for immutable set operations */
trait SetLike[A, +C[X] <: Set[X]]
  extends strawman.collection.SetLike[A, C]
    with SetOps[A, C[A]]

trait SetOps[A, +Repr] {

  def + (elem: A): Repr

  def - (elem: A): Repr

}