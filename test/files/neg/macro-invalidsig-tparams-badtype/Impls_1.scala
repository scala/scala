import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[U](c: Ctx)(U: c.mirror.Type) = ???
}
