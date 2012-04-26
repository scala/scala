import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[U: c.TypeTag](c: Ctx) = ???
}
