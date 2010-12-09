class Foo[T](v: T) {}

object Test {
 new Foo[Boolean](Boolean.FALSE)
}