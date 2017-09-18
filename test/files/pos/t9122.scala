class X[A](a: A)
object Test {
  implicit val ImplicitBoolean: Boolean = true
  def local = {
    implicit object X extends X({ def local2 = implicitly[Boolean] ; "" })
    implicitly[X[String]]
  }
}
