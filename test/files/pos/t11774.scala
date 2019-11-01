class C[T](x: AnyRef, y: Boolean = false) {
  def this(x: T) = this(x.asInstanceOf[AnyRef])
}
