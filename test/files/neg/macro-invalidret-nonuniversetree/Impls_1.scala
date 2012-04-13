import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx) = scala.reflect.mirror.Literal(scala.reflect.mirror.Constant(42))
}
