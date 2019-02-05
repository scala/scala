class Outer { class Inner }
object Test {
  def newOuter = new Outer
  val a: newOuter.Inner = { val o = newOuter; new o.Inner }
  val b: newOuter.Inner = a

  val o = newOuter
  val c: o.Inner = b
  val d: o.Inner = new o.Inner
  val e: o.Inner = d

  val f = new newOuter.Inner
}
