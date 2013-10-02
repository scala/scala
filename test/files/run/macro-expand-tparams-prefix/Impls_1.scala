import scala.reflect.runtime.universe._
import scala.reflect.macros.Context

object Impls1 {
  def foo[U: c.WeakTypeTag](c: Context)(x: c.Expr[U]) = {
    import c.universe._
    val U = implicitly[c.WeakTypeTag[U]]
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(U.toString))))
    c.Expr[Unit](body)
  }
}

object Impls2 {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Context)(x: c.Expr[U]) = {
    import c.universe._
    val T = implicitly[c.WeakTypeTag[T]]
    val U = implicitly[c.WeakTypeTag[U]]
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(T.toString + " " + U.toString))))
    c.Expr[Unit](body)
  }
}

object Impls345 {
  def foo[T, U: c.WeakTypeTag, V](c: Context)(implicit T: c.WeakTypeTag[T], V: c.WeakTypeTag[V]): c.Expr[Unit] = {
    import c.universe._
    c.Expr(Block(List(
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(T.toString)))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(implicitly[c.WeakTypeTag[U]].toString)))),
      Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(V.toString))))),
      Literal(Constant(()))))
  }
}

object Macros4 {
  class D[T] {
    class C[U] {
      def foo[V] = macro Impls345.foo[T, U, V]
    }
  }
}
