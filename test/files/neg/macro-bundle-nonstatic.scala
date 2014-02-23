import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class Module {
  class Bundle(val c: Context) {
    import c.universe._
    def impl = q"()"
  }
}

object Macros1 {
  def foo1 = macro Module.Bundle.impl
  def foo2 = macro new Module().Bundle.impl
}

object Macros2 extends Module {
  def foo = macro Bundle.impl
}

object Macros3 {
  val module = new Module
  import module._
  def foo = macro Bundle.impl
}

object Module {
  class GoodBundle(val c: Context) {
    import c.universe._
    def impl = q"()"
  }
}

object Macros4 {
  import Module._
  def foo: Unit = macro GoodBundle.impl
}
