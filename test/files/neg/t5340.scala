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

  // compilation fails with error below
  println(b: s.E)

  // amb prefix: MyApp.s.type#class E MyApp.r.type#class E
  // amb prefix: MyApp.s.type#class E MyApp.r.type#class E
  // ../test/pending/run/t5310.scala:17: error: type mismatch;
  //  found   : MyApp.r.E
  //  required: MyApp.s.E
  //   println(b: s.E)
  //           ^

  // The type error is as expected, but the `amb prefix` should be logged,
  // rather than printed to standard out.
}
