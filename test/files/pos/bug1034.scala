object Terminal {
  def apply[a] : a => unit = { a => () }
  val i0 = Terminal.apply[int]
  val i1 = (Terminal)[int]
  val i2 = Terminal[int]
}
