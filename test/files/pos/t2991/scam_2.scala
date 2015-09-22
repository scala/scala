
// was ambiguous to Scala
trait Scam {
  val j = new Jamb_1
  val o = new Object
  def f = j.f(o)        // yes, we can
  def g = j.g(42)       // ditto
  def h = j.g(42, 43)   // you better
  def k = j.j(42)       // threesome

  def z = j.k(o)        // picks the one-arg version
  def y = j.k()         // just checking
  def x = j.k(o, o)     // just checking
}
