import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[U: c.TypeTag: Numeric](c: Ctx) = {
    import c.mirror._
    Literal(Constant(42))
  }
}
