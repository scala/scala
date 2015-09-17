trait VectorLike[+T, +V[A] <: Vector[A]] {
  def +[S, VResult[S] >: V[S]](v: VResult[S])
}

trait Vector[+T] extends VectorLike[T, Vector]
trait ImmutableVector[T] extends Vector[T] with VectorLike[T, ImmutableVector]
trait MutableVector[T] extends Vector[T] with VectorLike[T, MutableVector]

object Test {
  def f = (null: MutableVector[Int]) + (null: ImmutableVector[Int])
}
