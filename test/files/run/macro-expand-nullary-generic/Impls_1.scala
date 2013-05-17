import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def impl[T: c.WeakTypeTag](c: Ctx) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), TermName("println")), List(Literal(Constant("it works " + implicitly[c.WeakTypeTag[T]]))))
    c.Expr[Unit](body)
  }

  def fooNullary[T: c.WeakTypeTag](c: Ctx) = impl[T](c)
  def fooEmpty[T: c.WeakTypeTag](c: Ctx)() = impl[T](c)
  def barNullary[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[Int]) = impl[T](c)
  def barEmpty[T: c.WeakTypeTag](c: Ctx)(x: c.Expr[Int])() = impl[T](c)
}