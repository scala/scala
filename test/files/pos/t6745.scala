class Bar(val i: Int) {
  self: Any with AnyRef =>
  def this() = this(0)
}
