object Test {
  sealed trait Foo[+A] // sealed or not doesn't matter
  case class ImplA[A](a: A) extends Foo[A]
  case class ImplAny[A](a: Any) extends Foo[A]

  trait Bar[+G[_]] // must be covariant

  def err[F[_]](): Unit = {
    val x: Foo[Foo[Bar[F]]] = ???

    x match {
      case ImplAny(ImplAny(_)) => ???
      case ImplAny(ImplA(_)) => ???
      case ImplA(_) => ???
      case ImplAny(_) => ???
      case _ => ???
    }

    x match {
      case ImplA(ImplA(_)) => ???
      case ImplA(ImplAny(_)) => ???
      case ImplA(y) => y.toString
      case ImplA(y) => y match {
        case ImplA(_) => ???
        case _ => ???
      }
      case ImplA(y) => y
      case _ => ???
    }

    ()
  }
}
