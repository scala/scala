import language.experimental.macros
import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = ???
}

trait Test {
  type X = { type Foo = macro Impls.impl }
}