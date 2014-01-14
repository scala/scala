import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[String]): c.Expr[Option[Int]] = {
    import c.universe._
    val body = Apply(Ident(definitions.SomeModule), List(Select(x.tree, TermName("toInt"))))
    c.Expr[Option[Int]](body)
  }
}