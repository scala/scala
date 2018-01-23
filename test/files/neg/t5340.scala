class Quux[T, U]

class Poly {
  class E
  object E {
    implicit def conv[T, U](b: Quux[T, U]): Int = 1
  }
}

object MyApp {
  val r: Poly = ???
  val s: Poly = ???

  (new Quux[r.E, Int]): Int // ok
  (new Quux[r.E, s.E]): Int // fails due to pre-stripping implicits which
                            // are reachable via different prefixes but not
                            // dependent on the prefix. Ambiguity not
                            // reported as such.
}
