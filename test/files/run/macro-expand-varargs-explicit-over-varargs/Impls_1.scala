import scala.reflect.macros.blackbox.Context

object Impls {
  def myprintln(xs: Int*) = {
    println(xs)
  }

  def foo(c: Context)(xs: c.Expr[Int]*) = {
    import c.universe._
    val body = Apply(Select(Ident(TermName("Impls")), TermName("myprintln")), xs.map(_.tree).toList)
    c.Expr[Unit](body)
  }
}