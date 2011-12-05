// /scala/trac/4758/a.scala
// Fri Dec  2 13:41:54 PST 2011

package bar {
  // works
  trait M[F[_]]
  class S[X[_] <: M[X], A](val x:X[A])
  object S {
    def apply[X[_] <: M[X], A](x: X[A]): S[X, A] = new S[X, A](x)
    def unapply[X[_] <: M[X], A](p: S[X, A]) = Some(p.x)
  }
}
package foo {
  // seemingly equivalent, doesn't work
  trait M[F[_]]
  case class S[X[_] <: M[X], A](x: X[A])
}
