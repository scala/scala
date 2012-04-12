import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo_targs[T, U: c.TypeTag](c: Ctx)(implicit x: c.Expr[Int]) = {
    import c.{prefix => prefix}
    import c.mirror._
    val body = Block(
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("invoking foo_targs...")))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("type of prefix is: " + prefix.tpe)))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("U is: " + implicitly[c.TypeTag[U]].tpe)))),
      Literal(Constant(())))
    Expr[Unit](body)
  }
}

class Macros[T] {
  def foo_targs[U](x: Int) = macro Impls.foo_targs[T, U]
}
