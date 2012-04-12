import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx)(x: c.Expr[String]): c.Expr[Option[Int]] = {
    import c.mirror._
    val body = Apply(Ident(definitions.SomeModule), List(Select(x.tree, newTermName("toInt"))))
    Expr[Option[Int]](body)
  }
}
