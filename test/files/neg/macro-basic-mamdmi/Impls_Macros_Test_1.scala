import scala.reflect.macros.blackbox.Context

object Impls {
  def foo(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"$x + 1")
  }

  def bar(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"$x + 2")
  }

  def quux(c: Context)(x: c.Expr[Int]): c.Expr[Int] = {
    import c.universe._
    c.Expr[Int](q"$x + 3")
  }
}

object Macros {
  object Shmacros {
    def foo(x: Int): Int = macro Impls.foo
  }
  def bar(x: Int): Int = macro Impls.bar
}

class Macros {
  def quux(x: Int): Int = macro Impls.quux
}

object Test extends App {
  import Macros.Shmacros._
  println(foo(2) + Macros.bar(2) * new Macros().quux(4))
}