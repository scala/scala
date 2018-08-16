object Test {
  case class Foo[A](self: A) { def bar: self.type = self }
  lazy val result = Foo(1).bar
}
