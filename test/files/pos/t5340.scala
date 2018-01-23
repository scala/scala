class Poly {
  class E
  object E {
    implicit def conv(value: Any): E = sys.error("")
  }
}

object MyApp {
  val r: Poly = sys.error("")
  val s: Poly = sys.error("")
  val b: r.E = sys.error("")

  // okay
  s.E.conv(b): s.E

  // okay
  println(b: s.E)
}
