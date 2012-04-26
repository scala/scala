import scala.reflect.makro.{Context => Ctx}

object Impls {
  def impl[T: c.TypeTag](c: Ctx) = {
    import c.mirror._
    val body = Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Literal(Constant("it works " + implicitly[c.TypeTag[T]]))))
    Expr[Unit](body)
  }

  def fooNullary[T: c.TypeTag](c: Ctx) = impl[T](c)
  def fooEmpty[T: c.TypeTag](c: Ctx)() = impl[T](c)
  def barNullary[T: c.TypeTag](c: Ctx)(x: c.Expr[Int]) = impl[T](c)
  def barEmpty[T: c.TypeTag](c: Ctx)(x: c.Expr[Int])() = impl[T](c)
}
