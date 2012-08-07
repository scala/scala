import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T, U: c.AbsTypeTag, V](c: Ctx)(implicit T: c.AbsTypeTag[T], V: c.AbsTypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    Block(List(
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(T.toString)))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(implicitly[c.AbsTypeTag[U]].toString)))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(V.toString))))),
      Literal(Constant(())))
  }
}