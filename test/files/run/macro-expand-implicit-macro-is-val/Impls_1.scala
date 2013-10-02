import scala.reflect.macros.{BlackboxContext => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.universe._
    val body = Literal(Constant(2))
    c.Expr[Int](body)
  }
}