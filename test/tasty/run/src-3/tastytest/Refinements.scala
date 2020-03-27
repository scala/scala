package tastytest

import scala.reflect.ClassTag

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
    def poly[T]: Any
    def polyNillary[T](): Any
    val value: Any
  }

  class Blip[A, M <: Methodic { def nullary: A } ] {
    def blip(m: M): A = m.nullary
  }

  class Blap[A, M <: Methodic { def nillary(): A } ] {
    def blap(m: M): A = m.nillary()
  }

  class Blam[A, M <: Methodic { val value: A } ] {
    def blam(m: M): A = m.value
  }

  class Bloc[A, M <: Methodic { def poly[T]: A } ] {
    def bloc(m: M): A = m.poly
  }

  class Blaa[A, M <: Methodic { def polyNillary[T](): A } ] {
    def blaa(m: M): A = m.polyNillary()
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
    def cloc[M <: Methodic { def poly[T]: A }](m: M): A = m.poly
  }

  class Claa[A] {
    def claa[M <: Methodic { def polyNillary[T](): A }](m: M): A = m.polyNillary()
  }

  trait MethodicComplex {
    def one(a: String): Any
    def two(a: String)(b: Boolean): Any
    def three[C](a: String)(b: Boolean)(c: C): Any
    def implicitly(implicit ls: List[String]): Any
    def contextual(using ls: List[String]): Any
  }

  class MethodOrPoly1[A, M <: MethodicComplex { def one(a: String): A } ] {
    def read(m: M, s: String): A = m.one(s)
  }

  class MethodOrPoly2[A, M <: MethodicComplex { def two(a: String)(b: Boolean): A } ] {
    def read(m: M, s: String, b: Boolean): A = m.two(s)(b)
  }

  class MethodOrPoly3[A, M <: MethodicComplex { def three[C](a: String)(b: Boolean)(c: C): A } ] {
    def read[C](m: M, s: String, b: Boolean, c: C): A = m.three(s)(b)(c)
  }

  class MethodOrPoly4[A, M <: MethodicComplex { def implicitly(implicit ls: List[String]): A } ] {
    def read(m: M)(implicit ls: List[String]): A = m.implicitly
  }

  class MethodOrPoly5[A, M <: MethodicComplex { def contextual(using ls: List[String]): A } ] {
    def read(m: M)(using ls: List[String]): A = m.contextual
  }

  class MethodOrPoly4_2[A] {
    def read[M <: MethodicComplex { def implicitly(implicit ls: List[String]): A }](m: M)(implicit ls: List[String]): A = m.implicitly
  }

  class MethodOrPoly5_2[A] {
    def read[M <: MethodicComplex { def contextual(using ls: List[String]): A }](m: M)(using ls: List[String]): A = m.contextual
  }

  class Structural1[M <: { val output: Int } ] {
    import reflect.Selectable.reflectiveSelectable
    def get(m: M): Int = m.output
  }

  class Structural2[M <: { def output: Int } ] {
    import reflect.Selectable.reflectiveSelectable
    def get(m: M): Int = m.output
  }

  class Structural3[M <: { def encode(t: Int): String } ] {
    import reflect.Selectable.reflectiveSelectable
    def encodeWith(m: M, t: Int): String = m.encode(t)
  }

  class StructuralSelectable[M <: Selectable { def encode(t: Int): String } ] {
    def encodeWith(m: M, t: Int): String = m.encode(t)
  }

  class StructuralFlip1 {
    import reflect.Selectable.reflectiveSelectable
    def get[M <: { val output: Int } ](m: M): Int = m.output
  }

  class StructuralFlip2 {
    import reflect.Selectable.reflectiveSelectable
    def get[M <: { def output: Int } ](m: M): Int = m.output
  }

  class StructuralFlip3 {
    import reflect.Selectable.reflectiveSelectable
    def encodeWith[M <: { def encode(t: Int): String } ](m: M, t: Int): String = m.encode(t)
  }

  class StructuralFlip4 {
    import reflect.Selectable.reflectiveSelectable
    def get(m: { val output: Int }): Int = m.output
  }

  class StructuralFlip5 {
    import reflect.Selectable.reflectiveSelectable
    def get(m: { def output: Int }): Int = m.output
  }

  class StructuralFlip6 {
    import reflect.Selectable.reflectiveSelectable
    def encodeWith(m: { def encode(t: Int): String }, t: Int): String = m.encode(t)
  }

  class StructuralTypeAliasFlip {
    import reflect.Selectable.reflectiveSelectable
    def get(ref: { type T = String; val t: T }): ref.T = ref.t
  }

  class StructuralTypeBoundsFlip {
    import reflect.Selectable.reflectiveSelectable
    def get(ref: { type T <: String; val t: T }): ref.T = ref.t
  }

  class StructuralSelectableFlip {
    def encodeWith[M <: Selectable { def encode(t: Int): String } ](m: M, t: Int): String = m.encode(t)
  }

}
