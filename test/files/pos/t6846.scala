object Test {
  class Arb[_]
  implicit def foo[M[_], A]: Arb[M[A]] = null
  foo: Arb[List[Int]]
  type ListInt = List[Int]
  foo: Arb[ListInt]
}

object Test2 {
  import scala.collection.immutable.List

  class Carb[_]
  implicit def narrow[N, M[_], A](x: Carb[M[A]])(implicit ev: N <:< M[A]): Carb[N] = null
  implicit def bar[M[_], A]: Carb[M[A]] = null

  type ListInt = List[Int]

  val x: List[Int] = List(1)
  val y: ListInt = List(1)

  type ListSingletonX = x.type
  type ListSingletonY = y.type

  bar: Carb[List[Int]]
  bar: Carb[ListInt]
  bar: Carb[ListSingletonX]
  bar: Carb[ListSingletonY]
}
