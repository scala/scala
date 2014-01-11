import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class X { type T }

object X {
  // this works
  def foo(x: X): x.T = macro fooImpl
  def fooImpl(c: Context)(x: c.Expr[X]): c.Expr[x.value.T] = ???

  // this doesn't
  def bar(x: X, y: X): (x.T, y.T) = macro barImpl
  def barImpl(c: Context)(x: c.Expr[X], y: c.Expr[X]): c.Expr[(x.value.T, y.value.T)] = ???

  // neither does this
  def baz(x: X)(xs: List[x.T]): Unit = macro bazImpl
  def bazImpl(c: Context)(x: c.Expr[X])(xs: c.Expr[List[x.value.T]]): c.Expr[Unit] = ???
}
