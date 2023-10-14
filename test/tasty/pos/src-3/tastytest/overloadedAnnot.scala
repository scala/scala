package tastytest

final class overloadedAnnot(str: String, int: Int, boolean: Boolean) extends scala.annotation.StaticAnnotation {
  def this(int: Int) = this("abc", int, false)
  def this(boolean: Boolean, str: String) = this(str, 123, boolean)
}
