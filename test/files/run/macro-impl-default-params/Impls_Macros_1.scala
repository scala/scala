import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo_targs[T, U: c.WeakTypeTag](c: Ctx = null)(x: c.Expr[Int] = null) = {
    import c.{prefix => prefix}
    import c.universe._
    val U = implicitly[c.WeakTypeTag[U]]
    val body = Block(List(
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("invoking foo_targs...")))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("type of prefix is: " + prefix.staticType)))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("type of prefix tree is: " + prefix.tree.tpe)))),
      Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("U is: " + U.tpe))))),
      Literal(Constant(())))
    c.Expr[Unit](body)
  }
}

class Macros[T] {
  def foo_targs[U](x: Int) = macro Impls.foo_targs[T, U]
}