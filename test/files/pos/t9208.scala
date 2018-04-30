
trait M

object O1 {
  implicit def global: M = ???
}

object O2 {
  implicit def global: M = ???
}

class Test {
  def test1 = {
    // the order of these imports in the same scope should not matter
    import O1._
    import O2.global
    global
    implicitly[M]
  }

  def test2 = {
    import O2.global
    import O1._
    global
    implicitly[M] // .. but, "error: could not find implicit value for parameter e: M"
  }
}
