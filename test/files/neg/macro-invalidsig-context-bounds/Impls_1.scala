import scala.reflect.runtime.universe._
import scala.reflect.macros.{Context => Ctx}

object Impls {
  def foo[U: c.WeakTypeTag: Numeric](c: Ctx) = {
    import c.universe._
    Literal(Constant(42))
  }
}