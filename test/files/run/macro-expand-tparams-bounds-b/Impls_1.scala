import scala.reflect.makro.{Context => Ctx}

class C

object Impls {
  def foo[U <: C](c: Ctx): c.Expr[Unit] = c.literalUnit
}
