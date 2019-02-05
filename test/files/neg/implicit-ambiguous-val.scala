sealed trait NotString[T]

object NotString extends NotString0 {
  @annotation.implicitAmbiguous("unexpected string")
  implicit val stringAmb_1: NotString[String] = null
  implicit val stringAmb_2: NotString[String] = null
}
sealed abstract class NotString0 {
  implicit def notString[T]: NotString[T] = null
}

object Test {
  def meh[T: NotString](t: T) = ()

  meh(12)
  meh("")
}