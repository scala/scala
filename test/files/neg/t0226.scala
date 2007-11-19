class Test {
  def foo[A](x: A)(implicit rep: Foo[A]): Foo[A] = rep
  abstract class Foo[A]
  implicit def list2Foo[List[A1], A2]
      (implicit _1: Foo[List[A1]], _2: Foo[A2]): Foo[Tuple2[List[A1], A2]] =
    null //dummy

  foo(((List('b'), 3), (Nil, 4)))
}
