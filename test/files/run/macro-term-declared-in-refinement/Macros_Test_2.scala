import scala.language.reflectiveCalls

class Base

object Test extends App {
  val macros = new Base { def foo: Unit = macro Impls.foo }
  macros.foo
}
