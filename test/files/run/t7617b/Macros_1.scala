import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context)(name: c.Expr[String])(value: c.Expr[Any]) = {
    import c.universe._
    reify(println(s"${name.splice} = ${value.splice}"))
  }
}