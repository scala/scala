object Test extends EmbeddedControls {
  trait Rep[T]
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
  val foo = new Struct {
    val a = 1 // a should be forced to have type Rep[Int]
  }
}
