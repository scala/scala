import scala.annotation.compileTimeOnly

class C(val s: String) extends AnyVal {
  @compileTimeOnly("error")
  def error = ???
}
