object Test {
  class Result[+A]
  case class Success[A](x: A) extends Result[A]
  class Apply[A]
  object Apply {
    def apply[A](f: Int => A): Apply[A] = new Apply[A]
  }

  def foo = Apply(i => i match {
    case 1 => Success(Some(1))
    case _ => Success(None)
  })
}
