//> using options -language:experimental.macros
//import scala.language.experimental.macros
import scala.language.reflectiveCalls

object Test extends App {
  val macros = new { def foo: Unit = macro Impls.foo }
  macros.foo
}
