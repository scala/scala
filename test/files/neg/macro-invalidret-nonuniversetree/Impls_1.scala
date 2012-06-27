import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = scala.reflect.basis.Literal(scala.reflect.basis.Constant(42))
}
