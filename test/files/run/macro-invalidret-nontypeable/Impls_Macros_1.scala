import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.universe._
    val body = Ident("IDoNotExist")
    c.Expr[Int](body)
  }
}

object Macros {
  def foo = macro Impls.foo
}