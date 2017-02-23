object Test extends EmbeddedControls {
  class Foo {
    type JSLiteral <: Struct
    case class Rep[T](x: T)
    def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
    val foo = new JSLiteral {
      val a = Rep[Int](1)
    }
  }
}
