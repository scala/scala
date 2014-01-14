import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context) = {
    import c.universe._
    val body = Literal(Constant(2))
    c.Expr[Int](body)
  }
}