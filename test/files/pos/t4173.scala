object t4173 {
  def bar(a: Int = 0, b: Int = 0)(cs: Any*) = ()
  bar(b = 1)(Nil: _*)
}
