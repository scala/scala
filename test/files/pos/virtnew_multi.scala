class Test extends EmbeddedControls {
  type Rep[T]
  def __new[T](args: (String, Rep[T] => Rep[_])*): Rep[T] = error("")
  //tests if it's possible to create two anynomous classes for Struct
  //and if they names do not clash
  val foo = new Struct {}
  val foo2 = new Struct {}
}
