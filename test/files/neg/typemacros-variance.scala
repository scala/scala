import language.experimental.macros
import scala.reflect.macros.Context

object Impls {
  def impl(c: Context) = ???
}

trait Test {
  type Foo[+T] = macro Impls.impl
  type Bar[-T] = macro Impls.impl
}