// After SI-7475 has enforced the rule that private members aren't
// inherited, we need to make pattern typing less eager to eliinate
// the "redundant" part of `d: D with C`. Otherwhise, `foo` is not
// a member of `d.type`.
class C {
  private def foo: Any = 0
  this match {
    case d: D =>
      d.foo
  }
}
object C {
  def test(c: C) = c match {
    case d: D =>
      d.foo
  }
}

class D extends C

object Other {
  def test(c: C) = c match {
    case d: D =>
      d // here, as before, we can eliminate `C`.
  }
}
