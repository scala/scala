object Test {
  trait Seq[+t] {
    type MyType[+t] <: Seq[t]
    def f: MyType[t]
  }

  def span[a, s <: Seq[a] { type MyType <: s } ](xs: s): s
    = xs f // xs: s <: Seq[a]{type MyType <: s }
    // xs.f : xs.MyType[a] <: Seq[a]
    // ill-formed type in bound for s: Seq[a] { type MyType <: s }
    // refinements aren't checked -- should they?
}