import scala.reflect.macros.Context

class C
object Impls {
  def impl(c: Context)(x: c.Expr[Int])(y: c.Expr[Int]) = {
    import c.universe._
    // Apply(Apply(Ident(TypeName("C")), List(x.tree)), List(y.tree))
    Ident(TypeName("C"))
  }
}