import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.mirror._
    val body = Literal(Constant(2))
    Expr[Int](body)
  }
}
