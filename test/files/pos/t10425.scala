class Poly {
  class E
}

object MyApp {
  object r extends Poly {
    implicit def conv(value: Any): E = sys.error("")
  }
  object s extends Poly {
    implicit def conv(value: Any): E = sys.error("")
  }
  val b: r.E = sys.error("")

  // okay
  s.conv(b): s.E

  // okay
  println(b: s.E)
}
