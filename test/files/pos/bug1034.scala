object Terminal {
  def apply[a] : a => Unit = { a => () }
  val i0 = Terminal.apply[Int]
  val i1 = (Terminal)[Int]
  val i2 = Terminal[Int]
}
