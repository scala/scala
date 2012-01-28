class Bar[F[_]] extends Foo[F] {
  def foo[G[_[_], _]](implicit M: M[G]): X[({type λ[α] = G[F, α] })#λ] = null
}
// vim: set ts=4 sw=4 et:

trait M[F[_[_], _]]
trait N[F[_], G[_]]

trait X[F[_]] {
  def apply[A]: F[A]
}

trait Foo[F[_]] {
  def foo[G[_]](implicit n: N[G, F]): X[F]
}
