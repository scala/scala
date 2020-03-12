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

  trait Methodic {
    def nullary: Any
    def nillary(): Any
    def poly[T](): Any
    val value: Any
  }

  class Blip[A, M <: Methodic { def nullary: A } ] {
    def blip(m: M): A = m.nullary
  }

  class Blap[A, M <: Methodic { def nillary(): A } ] { // generates METHODtype tag
    def blap(m: M): A = m.nillary()
  }

  class Blam[A, M <: Methodic { val value: A } ] {
    def blam(m: M): A = m.value
  }

  class Bloc[A, M <: Methodic { def poly[T](): A } ] { // generates POLYtype tag
    def bloc(m: M): A = m.poly()
  }

  class Clip[A] {
    def clip[M <: Methodic { def nullary: A }](m: M): A = m.nullary
  }

  class Clap[A] {
    def clap[M <: Methodic { def nillary(): A }](m: M): A = m.nillary()
  }

  class Clam[A] {
    def clam[M <: Methodic { val value: A }](m: M): A = m.value
  }

  class Cloc[A] {
    def cloc[M <: Methodic { def poly[T](): A }](m: M): A = m.poly()
  }

}
