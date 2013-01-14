object Macros { type Foo(x: Int) = macro Impls.foo }
import Macros._

object Test extends App {
  import Macros._

  class D1 extends Foo("42")
  val x1: Foo("42") = 42

  class D2 extends Foo
  val x2: Foo = 42

  class D3 extends Foo(4)(2)
  val x3: Foo(4)(2) = 42

  class D4 extends Foo()
  val x4: Foo() = 42

  class D5 extends Foo(4, 2)
  val x5: Foo(4, 2) = 42
}