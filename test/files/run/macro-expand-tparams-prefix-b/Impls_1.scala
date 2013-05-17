import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[T: c.WeakTypeTag, U: c.WeakTypeTag](c: Ctx)(x: c.Expr[U]) = {
    import c.universe._
    val T = implicitly[c.WeakTypeTag[T]]
    val U = implicitly[c.WeakTypeTag[U]]
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(T.toString + " " + U.toString))))
    c.Expr[Unit](body)
  }
}