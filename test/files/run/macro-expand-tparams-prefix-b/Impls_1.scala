import scala.reflect.runtime.universe._
import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[T: c.TypeTag, U: c.TypeTag](c: Ctx)(x: c.Expr[U]) = {
    import c.universe._
    val T = implicitly[c.TypeTag[T]]
    val U = implicitly[c.TypeTag[U]]
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant(T.toString + " " + U.toString))))
    c.Expr[Unit](body)
  }
}