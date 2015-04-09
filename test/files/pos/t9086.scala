class X[A](a: A)
object Test {
  implicit val ImplicitBoolean: Boolean = true
  def local = {
    implicit object X extends X({ implicitly[Boolean] ; "" })
    implicitly[X[String]] // failed in 2.11.5
  }
}
