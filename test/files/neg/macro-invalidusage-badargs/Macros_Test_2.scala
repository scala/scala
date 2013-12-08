object Macros { def foo(x: Int): Int = macro Impls.foo }
import Macros._

object Test extends App {
  foo("42")
  foo
  foo(4)(2)
  foo()
  foo(4, 2)
}