import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[T, U: c.TypeTag, V](c: Ctx)(implicit T: c.TypeTag[T], V: c.TypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    Block(List(
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(T.toString)))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(implicitly[c.TypeTag[U]].toString)))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(V.toString))))),
      Literal(Constant(())))
  }
}