package test
class Lst[T]
case class cons[T](x: T, xs: Lst[T]) extends Lst[T]
case class nil[T]() extends Lst[T]
object test {
  Console.println(cons(1, nil()))
}
