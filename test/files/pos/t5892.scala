class foo(a: String) extends annotation.StaticAnnotation
object o {
  implicit def i2s(i: Int) = ""
  @foo(1: String) def blerg { }
}
