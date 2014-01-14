import scala.reflect.macros.blackbox.Context

object Impls {
  def foo[T, U: c.WeakTypeTag, V](c: Context)(implicit T: c.WeakTypeTag[T], V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    Block(List(
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(T.toString)))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(implicitly[c.WeakTypeTag[U]].toString)))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(V.toString))))),
      Literal(Constant(())))
  }
}