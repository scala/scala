import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[U: c.WeakTypeTag](c: Ctx)(x: c.Expr[U]) = {
    import c.universe._
    val U = implicitly[c.WeakTypeTag[U]]
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant(U.toString))))
    c.Expr[Unit](body)
  }
}