import scala.reflect.macros.Context

class C(val x: Int)
object Impls {
  def impl(c: Context)(x: c.Expr[Int]) = {
    import c.universe._
    Apply(Ident(TypeName("C")), List(x.tree))
  }
}
