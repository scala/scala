
object Test {
  def +[T](x: T): String = "x"
  +[Int](6): String   // OK in scala 2
}
class C {
  def i = `+` 42      // error not taken as unary prefix operator
}
