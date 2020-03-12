package tastytest

object Refinements {

  trait Foo {
    type T
    type U
    def foo: (T, U)
  }

  trait FooT {
    type T
    def fooT: T
  }

  trait FooU {
    type U
    def fooU: U
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

  class Qux[A, B, F <: FooT with FooU { type T = A; type U = B }] {
    def qux(member: F): (member.T, member.U) = {
      (member.fooT, member.fooU)
    }
  }

  class Zot[A, B] {
    def zot[F <: FooT with FooU { type T = A; type U = B } ](member: F): (member.T, member.U) = {
      (member.fooT, member.fooU)
    }
  }

}
