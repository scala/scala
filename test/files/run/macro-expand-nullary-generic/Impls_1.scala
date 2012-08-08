import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def impl[T: c.AbsTypeTag](c: Ctx) = {
    import c.universe._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("it works " + implicitly[c.AbsTypeTag[T]]))))
    c.Expr[Unit](body)
  }

  def fooNullary[T: c.AbsTypeTag](c: Ctx) = impl[T](c)
  def fooEmpty[T: c.AbsTypeTag](c: Ctx)() = impl[T](c)
  def barNullary[T: c.AbsTypeTag](c: Ctx)(x: c.Expr[Int]) = impl[T](c)
  def barEmpty[T: c.AbsTypeTag](c: Ctx)(x: c.Expr[Int])() = impl[T](c)
}