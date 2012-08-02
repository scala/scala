import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = {
    import c.universe._
    println(c.fresh())
    println(c.fresh("qwe"))
    println(c.fresh(newTypeName("qwe")))
    c.abort(NoPosition, "blargh")
  }
}

object Macros {
  def foo = macro Impls.impl
}