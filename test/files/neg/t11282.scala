// These all used to be crashers; now they're just errors.
object t11282 {
  def a[T] = ()
  a().fail[scala.Int] // error
  def b[F[_]] = ()
  b().fail[scala.Int] // error
}
object t11333 {
  Map().empty.asInstanceOf[String, (String, String)] // error
}
