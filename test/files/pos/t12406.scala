// scalac: -Werror
object Scabug {
  def foo[T : scala.reflect.ClassTag](bar: Either[String, T]): Boolean = bar match {
    case Left(_) => false
    case Right(_: T) => true
  }
}
