import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[T: c.TypeTag](c: Ctx)(x: c.Expr[T]) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(x.tree.toString))))
    c.Expr[Unit](body)
  }
}

object Macros {
  def foo[T](x: T) = macro Impls.foo[T]
}