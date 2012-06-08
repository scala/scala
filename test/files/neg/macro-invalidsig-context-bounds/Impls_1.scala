import scala.reflect.runtime.universe._
import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[U: c.TypeTag: Numeric](c: Ctx) = {
    import c.universe._
    Literal(Constant(42))
  }
}