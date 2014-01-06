import scala.reflect.macros.BlackboxContext

object Macros {
  def impl(c: BlackboxContext)(name: c.Expr[String])(value: c.Expr[Any]) = {
    import c.universe._
    reify(println(s"${name.splice} = ${value.splice}"))
  }
}