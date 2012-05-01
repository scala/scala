import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[U <: String](c: Ctx): c.Expr[Unit] = c.literalUnit
}
