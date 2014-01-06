package p1 {
  object Ex  { def unapply(p: Any): Option[_ <: Int] = null }
  object Foo { val Ex(_) = null }
}
// a.scala:2: error: error during expansion of this match (this is a scalac bug).
// The underlying error was: type mismatch;
//  found   : Some[_$1(in value x$1)] where type _$1(in value x$1)
//  required: Some[_$1(in method unapply)]
// object Foo { val Ex(_) = null }
//                    ^
// one error found

package p2 {
  trait Other {
    class Quux
    object Baz { def unapply(x: Any): Option[Quux] = None }
  }
  trait Reifiers {
    def f() {
      val u2: Other = null
      (null: Any) match { case u2.Baz(x) => println(x) } //: u2.Quux) }
      // The underlying error was: type mismatch;
      //  found   : Other#Quux
      //  required: u2.Quux
      //     x match { case u2.Baz(x) => println(x: u2.Quux) }
      //       ^
      // one error found
    }
  }
}
