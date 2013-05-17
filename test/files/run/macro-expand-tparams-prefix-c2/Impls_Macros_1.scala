import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T, U: c.WeakTypeTag, V](c: Ctx)(implicit T: c.WeakTypeTag[T], V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(Block(List(
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(T.toString)))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(implicitly[c.WeakTypeTag[U]].toString)))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(V.toString))))),
      Literal(Constant(()))))
  }
}

class D[T] {
  class C[U] {
    def foo[V] = macro Impls.foo[T, U, V]
  }
}