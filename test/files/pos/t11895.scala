//> using options -Werror -deprecation
//
trait Bar {
  trait Foo
  val Foo: Foo
}
object SomeBar extends Bar {
  object Foo extends Foo
}
