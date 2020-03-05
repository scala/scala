// scalac: -unchecked -Xfatal-warnings
//
object Test {
  def hasMatch[T](x: AnyRef) = x.isInstanceOf[T]
}
