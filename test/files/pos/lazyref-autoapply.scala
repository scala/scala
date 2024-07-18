//> using options -Werror -Xsource:3 -Xlint

object Test {
  def doti(i: Int): Product = {
    case class Dot(i: Int) // was: warning: auto-application to `()` is deprecated ...
    Dot(i)
  }
}
object t11890 {
  // was: warning: Auto-application to `()` is deprecated. Supply the empty argument list `()` explicitly to invoke method unary_!, ...
  lazy val x = 5
}
