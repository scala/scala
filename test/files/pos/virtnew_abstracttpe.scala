object Test {

  trait Base extends EmbeddedControls {
    type Rep[T]
    type JSLiteral <: Struct
  }

  object Specific extends Base {
    def __new[T](args: (String, Rep[T] => Rep[_])*): Rep[T] = error("")
    val foo = new JSLiteral { // it should be possible to call new on abstract type T as long as T <: Struct
    }
  }
}
