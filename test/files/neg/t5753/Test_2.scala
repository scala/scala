import scala.reflect.macros.{Context => Ctx}

object Macros extends Impls {
 def foo(x: Any) = macro impl
}

object Test extends App {
 import Macros._
 println(foo(42))
}

