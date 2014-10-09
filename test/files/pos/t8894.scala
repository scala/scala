class CC(val i: Int, val s: String)
object CC extends {
  type P = (Int, String)

  //def unapply(c: CC): Option[(Int, String)] = Some((c.i, c.s)) // OK
  def unapply(c: CC): Option[P] = Some((c.i, c.s)) // fails (because of the type alias)
}

class Test {
  val cc = new CC(23, "foo")
  val CC(i, s) = cc
}