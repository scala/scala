class Bar(i: Any) {
  self: Any =>
  def this() = this(0) // fail, remove `: Any` from self type to make it work.

  new Bar(0) // okay
}
