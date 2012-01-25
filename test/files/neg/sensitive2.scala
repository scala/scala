object Test {
  class Foo[A](z: A)
  implicit def foo1[A](a: A): Foo[A] = new Foo(a)
  implicit def foo2(a: Any): Foo[String] = new Foo("123")

  val a: Foo[_] = "abc"

}