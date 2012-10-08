class X(val i: Int) extends AnyVal {
  class Inner(val q: Int) {
    def plus = i + q
  }
}

object Test extends App {
  val x = new X(11)
  val i = new x.Inner(22)
  assert(i.plus == 33)
}
