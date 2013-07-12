import scala.reflect.macros.Context

object Macros {
  def impl(c: Context)(name: c.Expr[String])(value: c.Expr[Any]) = {
    import c.universe._
    reify(println(s"${name.splice} = ${value.splice}"))
  }
}