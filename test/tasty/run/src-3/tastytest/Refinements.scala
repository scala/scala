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

  trait Box {
    val value: Any
  }

  class PreciseRefinement[B <: Box { val value: String }] {
    def identity(b: B): b.type = b
    def valueOf(b: B): b.value.type = b.value
  }

  trait SpecialRefinement {
    def pickOne(as: String*): Option[Any]
    def eval(as: => String): Any
  }

  class EvalSpecialRefinement_1[S <: SpecialRefinement { def eval(as: => String): String }] {
    def run(s: S, as: => String): String = s.eval(as)
  }

  class EvalSpecialRefinement_2 {
    def run[S <: SpecialRefinement { def eval(as: => String): String }](s: S, as: => String): String = s.eval(as)
  }

  class PickOneRefinement_1[S <: SpecialRefinement { def pickOne(as: String*): Option[String] }] {
    def run(s: S, as: String*): Option[String] = s.pickOne(as:_*)
  }

  class PickOneRefinement_2 {
    def run[S <: SpecialRefinement { def pickOne(as: String*): Option[String] }](s: S, as: String*): Option[String] =
      s.pickOne(as:_*)
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

  final class MethodSelectable(methods: MethodSelectable.Method*)
  extends scala.Selectable {
    private val map = methods.toMap

    def applyDynamic(name: String, paramClasses: Class[_]*)(args: Any*): Any =
      map.get(name, paramClasses) match
        case Some(f) => f(args)
        case None    => throw NoSuchMethodException(s"$name(${paramClasses.mkString(",")})")

  }
  object MethodSelectable {
    type Method = ((String, Seq[Class[_]]), Seq[Any] => Any)
    def method(name: String, paramClasses: Class[_]*)(impl: Seq[Any] => Any): Method =
      ((name, paramClasses), impl)
  }

  class StructuralSelectable[M <: MethodSelectable { def encode(t: Int): String } ] {
    def encodeWith(m: M, t: Int): String = m.encode(t)
  }

  class StructuralFlip1 {
    def get[M <: { val output: Int } ](m: M): Int = {
      import reflect.Selectable.reflectiveSelectable
      m.output
    }
  }

  class StructuralFlip2 {
    def get[M <: { def output: Int } ](m: M): Int = {
      import reflect.Selectable.reflectiveSelectable
      m.output
    }
  }

  class StructuralFlip3 {
    def encodeWith[M <: { def encode(t: Int): String } ](m: M, t: Int): String = {
      import reflect.Selectable.reflectiveSelectable
      m.encode(t)
    }
  }

  class StructuralFlip4 {
    def get(m: { val output: Int }): Int = {
      import reflect.Selectable.reflectiveSelectable
      m.output
    }
  }

  class StructuralFlip5 {
    def get(m: { def output: Int }): Int = {
      import reflect.Selectable.reflectiveSelectable
      m.output
    }
  }

  class StructuralFlip6 {
    def encodeWith(m: { def encode(t: Int): String }, t: Int): String = {
      import reflect.Selectable.reflectiveSelectable
      m.encode(t)
    }
  }

  class StructuralTypeAliasFlip {
    def get(ref: { type T = String; val t: T }): ref.T = {
      import reflect.Selectable.reflectiveSelectable
      ref.t
    }
  }

  class StructuralTypeBoundsFlip {
    def get(ref: { type T <: String; val t: T }): ref.T = {
      import reflect.Selectable.reflectiveSelectable
      ref.t
    }
  }

  class StructuralSelectableFlip {
    def encodeWith[M <: MethodSelectable { def encode(t: Int): String } ](m: M, t: Int): String = m.encode(t)
  }

}
