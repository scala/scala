
// was ambiguous to Scala
trait Scam {
  val j = new Jamb_1
  val o = new Object
  def f = j.f(o)        // yes, we can
  def g = j.g(42)       // ditto
  def k = j.j(42)       // threesome
}
