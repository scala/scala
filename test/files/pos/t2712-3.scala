package test

object Test1 {
  class Foo[T, F[_]]
  def meh[M[_[_]], F[_]](x: M[F]): M[F] = x
  meh(new Foo[Int, List]) // solves ?M = [X[_]]Foo[Int, X[_]] ?A = List ...
}

object Test2 {
  trait TC[T]
  class Foo[F[_], G[_]]
  def meh[G[_[_]]](g: G[TC]) = ???
  meh(new Foo[TC, TC]) // solves ?G = [X[_]]Foo[TC, X]
}

object Test3 {
  trait TC[F[_]]
  trait TC2[F[_]]
  class Foo[F[_[_]], G[_[_]]]
  new Foo[TC, TC2]

  def meh[G[_[_[_]]]](g: G[TC2]) = ???
  meh(new Foo[TC, TC2]) // solves ?G = [X[_[_]]]Foo[TC, X]
}
