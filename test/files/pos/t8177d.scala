// exercise coevolveSym
trait HasElem { type A }
trait View[AIn] {
  val tc: HasElem { type A = AIn }
  def f2(p: tc.A): tc.A = p
}

object Test {
  val view: View[Int] = null

  view f2 5  // fails
}
