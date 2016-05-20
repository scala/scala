package test

object Test1 {
  trait X
  trait Y extends X
  class Foo[T, U <: X]
  def meh[M[_ <: A], A](x: M[A]): M[A] = x
  meh(new Foo[Int, Y])
}

object Test2 {
  trait X
  trait Y extends X
  class Foo[T, U >: Y]
  def meh[M[_ >: A], A](x: M[A]): M[A] = x
  meh(new Foo[Int, X])
}
