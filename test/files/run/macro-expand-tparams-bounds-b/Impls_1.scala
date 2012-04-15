import scala.reflect.makro.{Context => Ctx}

class C

object Impls {
  def foo[U <: C](c: Ctx): c.Expr[Unit] = {
    import c.mirror._
    Literal(Constant(()))
  }
}
