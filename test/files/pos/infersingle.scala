object Test {
  def one[T](x: T): Option[T] = Some(x)
  val x = "one"
  val y: Option[x.type] = one(x)
}