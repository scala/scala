
// When faced with ambiguities between imports,
// an attempt is made to see if the imports intend
// identical types.
//
// Here, no attempt is made to notice that x
// names the same thing, because the definition is in this file.
//
object X {
  val x = 42
  def f = {
    import X.x
    x
    // not OK, import doesn't shadow definition
  }
}

// counterexamples showing normal behavior, no puzzlers

object X2 {
  val x = 42
  def f = {
    def x = ???
    import X2.{x => x2}
    x2           // OK, rename makes it obvious there were some poor naming choices
  }
}

object Y {
  import Z._

  object Z {
    def z = 17
    def f = z     // OK, definition shadows import
  }
  object Other {
    def g = z     // the casually scoped import is useful
  }
}
