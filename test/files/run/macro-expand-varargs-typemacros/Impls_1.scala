import scala.reflect.macros.Context

class C_varargs(val xs: Int*)
class C(val x: Int, val y: Int, val z: Int, val u: Int, val w: Int)

object Impls {
  def impl_varargs(c: Context)(xs: c.Expr[Int]*) = {
    import c.universe._
    Apply(Ident(TypeName("C_varargs")), xs.map(_.tree).toList)
  }

  def impl(c: Context)(xs: c.Expr[Int]*) = {
    import c.universe._
    val xs1 = xs.map(_.tree).toList match {
      case List(Typed(list, Ident(wildstar))) if wildstar == tpnme.WILDCARD_STAR =>
        (1 to 5).map(i => Apply(list.duplicate, List(Literal(Constant(i))))).toList
      case xs => xs
    }
    Apply(Ident(TypeName("C")), xs1)
  }
}