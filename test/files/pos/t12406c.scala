// scalac: -Werror
object Scabug {
  def foo[T](bar: Either[String, T]): Boolean = bar match {
    case Left(_) => false
    case Right(_: T) => true
  }
}
