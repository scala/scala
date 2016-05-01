trait Day

object Test {
  def foo(t: Int with Day) = t == t
}

class DayOps(val i: Int with Day) extends AnyVal