import scala.reflect.makro.{Context => Ctx}

object Impls {
  def myprintln(xs: Int*) = {
    println(xs)
  }

  def foo(c: Ctx)(xs: c.Expr[Int]*) = {
    import c.mirror._
    val body = Apply(Select(Ident(newTermName("Impls")), newTermName("myprintln")), xs.map(_.tree).toList)
    Expr[Unit](body)
  }
}
