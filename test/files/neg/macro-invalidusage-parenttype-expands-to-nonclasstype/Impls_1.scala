import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[Int]) = {
    import c.universe._
    import Flag._
    val trivialTypeBounds = TypeBoundsTree(Ident(TypeName("Nothing")), Ident(TypeName("Any")))
    ExistentialTypeTree(Ident(TypeName("T")), List(TypeDef(Modifiers(DEFERRED), TypeName("T"), Nil, trivialTypeBounds)))
  }
}
