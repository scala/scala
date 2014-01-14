import scala.reflect.macros.blackbox.Context

object Macros1 {
  val impls = new Impls1
  def foo(x: Any) = macro impls.foo
}

object Macros2 {
  val impls = Impls2
  def foo(x: Any) = macro impls.foo
}

class Macros3 {
  object Impls3 {
    def foo(c: Context)(x: c.Expr[Any]) = ???
  }

  def foo(x: Any) = macro Impls3.foo
}

class Macros4 extends MacroHelpers {
  def foo(x: Any) = macro Impls4.foo
}

object Macros5 {
  def foo(x: Any) = macro Impls5.foo
  def foo(x: Any, y: Any) = macro Impls5.foo
}

object Macros6 {
  def foo1 = macro Impls6.fooEmpty
  def bar1() = macro Impls6.fooNullary
}

object Macros7 {
  def foo = macro Impls7.foo[String]
}

object Test extends App {
  println(Macros1.foo(42))
  println(Macros2.foo(42))
  println(new Macros3().foo(42))
  println(new Macros4().foo(42))
  println(Macros5.foo(42))
  println(Macros6.foo1)
  println(Macros6.bar1)
  println(Macros6.bar1())
  println(Macros7.foo)
}

package foo {
  object Test extends App {
    def foo = macro Impls8.impl
  }
}