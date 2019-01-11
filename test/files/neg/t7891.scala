class B { self =>
  def this(x: Any) { this() ; new Object { self } }
}