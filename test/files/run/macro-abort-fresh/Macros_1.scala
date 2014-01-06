import scala.reflect.macros.BlackboxContext

object Impls {
  def impl(c: BlackboxContext) = {
    import c.universe._
    println(c.fresh())
    println(c.fresh("qwe"))
    println(c.fresh(TypeName("qwe")))
    c.abort(NoPosition, "blargh")
  }
}

object Macros {
  def foo = macro Impls.impl
}