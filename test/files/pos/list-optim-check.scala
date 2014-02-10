// Tests a map known to crash in optimizer with faster List map in SI-8240.
// Equivalent tests for collect and flatmap do not crash, but are provided
// anyway.
// See ticket SI-8334 for optimizer bug.
// TODO - Remove this test once SI-8334 is fixed and has its own test.
class A {
  def f: Boolean = {
    val xs = Nil map (_ => return false)
    true
  }

  def g: Boolean = {
    val xs = Nil collect { case _ => return false }
    true
  }

  def h: Boolean = {
    val xs = Nil flatMap { _ => return false }
    true
  }
}
