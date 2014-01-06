// Tue Jul 12 16:38:23 PDT 2011

class Bip[T1]
class Foo[T2] extends Bip[T2]
class Bar[T3] extends Bip[T3]

abstract class Factory[CC[X] <: Bip[X]] { }

object Quux1 extends Factory[Foo]
object Quux2 extends Factory[Bar]

object Test {
  // FAIL
  val xs = List(Quux1, Quux2)
  //   error: type mismatch;
  // found   : Quux1.type (with underlying type object Quux1)
  // required: Factory[_ >: Bar with Foo <: Bip]
  //                   ^^     ^^       ^^     ^^ <-- QUIZ: what is missing from these types?

  // The type it should figure out, come on scalac
  type F = Factory[CC] forSome { type X ; type CC[X] >: Bar[X] with Foo[X] <: Bip[X] }

  // No problem
  val ys = List[F](Quux1, Quux2)

  // A repl session to get you started.
/*
  val quux1 = EmptyPackageClass.tpe.member(TermName("Quux1"))
  val quux2 = EmptyPackageClass.tpe.member(TermName("Quux2"))
  val tps   = List(quux1, quux2) map (_.tpe)
  val test  = EmptyPackageClass.tpe.member(TermName("Test"))
  val f     = test.tpe.member(TypeName("F")).tpe

  val fn    = f.normalize.asInstanceOf[ExistentialType]
  val fn2   = fn.underlying.asInstanceOf[TypeRef]
*/
}
