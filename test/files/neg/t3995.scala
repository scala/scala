class Lift {
  def apply(f: F0) {}

  class F0
  object F0 {
    implicit def f2f0(fn: String): F0 = ???
  }
}

object Test {
  val l = new Lift
  val f = ""

  "": l.F0 // okay

  l.apply("") // okay

  {
    val l = new Lift
    l.apply("") // okay
  }

  // fails trying to mkAttributedQualifier for pre = Skolem(_1 <: Lift with Singletom).F0
  // should this even have shown up in `companionImplicitMap`? It says that:
  //
  // "@return For those parts that refer to classes with companion objects that
  // can be accessed with unambiguous stable prefixes, the implicits infos
  // which are members of these companion objects."
  //
  // The skolem is stable, but it does not seem much good to us
  (new Lift).apply("")
}
