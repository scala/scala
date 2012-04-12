import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = {
    import c.mirror._
    val body = Ident("IDoNotExist")
    Expr[Int](body)
  }
}

object Macros {
  def foo = macro Impls.foo
}
