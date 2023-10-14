
object Test {
  def +[T](x: T): String = "x"
  `+`[Int](6): String   // Parser can treat + as identifier when backquoted and followed by a type argument
  `+`(6): String        // Parser can treat + as identifier when backquoted and followed by a value argument
  +(6): Int             // Parser prioritizes + as unary when possible
}
