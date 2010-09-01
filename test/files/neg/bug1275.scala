object Test {
  trait Seq[+t] {
    type MyType[+t] <: Seq[t]

    def f: MyType[t]
  }

  // illegal abstract type member refinement: changes the arity of MyType
  // the error is pretty strange, since the compiler forms the illegal type xs.MyType[a] anyway
  def span[a, s <: Seq[a] { type MyType/*look ma, no type parameters!*/ <: s } ](xs: s): s
     = xs f
//        ^
// found   : xs.MyType[a]
// required: s
}