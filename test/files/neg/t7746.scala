object Test {
  def f[T](x: Option[T]) = x match {
    case Some(Some(5)) => true
  }
}