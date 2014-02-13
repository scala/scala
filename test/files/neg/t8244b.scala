class Raw_1[T]{
  def raw(): Raw_1[_] = { new Raw_1[String] { def t() = "" } }
  def t(): T
}


class X extends Raw_1[X] {
  override def t = this
  def exxx = 0
}

object Test extends App {
  def c(s: X) = {
    val raw = s.raw
    raw.t.exxx
  }
  c(new X())
}
