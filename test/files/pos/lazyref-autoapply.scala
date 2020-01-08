// scalac: -Xsource:2.14

object Test {
  def doti(i: Int): Product = {
    case class Dot(i: Int) // was: warning: auto-application to `()` is deprecated ...
    Dot(i)
  }
}