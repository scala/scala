@annotation.inductive
trait Foo[T]
object Foo {
  implicit def fooInt: Foo[Int] = ???
}

object Test {
  implicitly[Foo[Int]]
}
