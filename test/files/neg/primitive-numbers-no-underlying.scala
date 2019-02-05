class PrimitiveNumbers(b: Byte, s: Short, c: Char, i: Int, l: Long, f: Float, d: Double) {
  def noUnderlying(): Unit = {
    b.underlying
    s.underlying
    c.underlying
    i.underlying
    l.underlying
    f.underlying
    d.underlying
  }
}
