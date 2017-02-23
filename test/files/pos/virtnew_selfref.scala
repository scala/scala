object Test extends EmbeddedControls {
  case class Rep[T](x: T) {
    def selectDynamic[T](field: String): Rep[T] = null.asInstanceOf[Rep[T]]
  }
  def __new[T](args: (String, Boolean, Rep[T] => Rep[_])*): Rep[T] = error("")
  val foo = new Struct {
    val a = Rep[Int](1)
    val b: Rep[Int] = a // it should be possible to refer a, b should have type Rep[Int]
  }
}
