class X extends Raw[X] {
  override def t = this
  def exxx = 0
}

object Test extends App {
  def c(s: X) = {
    val raw = s.raw
    raw.t.exxx // java.lang.ClassCastException: java.lang.String cannot be cast to X
  }
  c(new X())
}
