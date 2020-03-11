package tastytest

object Refinements {

  trait Foo {
    type T
    type U
    def foo: (T, U)
  }

  class Bar[A, B] {
    def bar[F <: Foo { type T = A; type U = B }](member: F): (member.T, member.U) = {
      member.foo
    }
  }

  class Baz[A, B, F <: Foo { type T = A; type U = B }] {
    def baz(member: F): (member.T, member.U) = {
      member.foo
    }
  }

}
