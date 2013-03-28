import scala.tools.partest.ReplTest

class A {
  sealed trait F[A]
}

class C[T] extends A {
  sealed trait F[A]
  object X {
    object S1 extends F[T]
  }
  class S2 extends F[T]
}
object O extends C[Int] {
  def foo(f: F[Int]) = f match { case X.S1 => }

  class S3 extends F[Int]
}
class S4 extends O.F[String]

object Test extends ReplTest {
  override def code = """
:power
val u = rootMirror.universe; import u._, language._
val S1 = typeOf[c.X.S1.type forSome { val c: C[_] }].typeSymbol.tpeHK
val S2 = typeOf[O.S2].typeSymbol.tpeHK
val S3 = typeOf[O.S3].typeSymbol.tpeHK
val S4 = typeOf[S4].typeSymbol.tpeHK
val F = typeOf[c.F[_] forSome { val c: C[_] }].typeSymbol.tpeHK
val fTpe = typeOf[O.type].decl(newTermName("foo")).paramss.head.head.tpe
def memType(sub: Type, scrut: Type): Type =
  nestedMemberType(sub.typeSymbol, scrut.prefix, scrut.typeSymbol.owner)

val mt1 = memType(S1, fTpe)
global.typeDeconstruct.show(mt1)
memType(S2, fTpe)
memType(S3, fTpe)
memType(S4, fTpe)
  """.stripMargin.trim
}