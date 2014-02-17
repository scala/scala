import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def foo_impl(c: Context)(xs: c.Tree*) = {
    import c.universe._
    q"println(${xs.toString})"
  }

  def foo(xs: Int*): Unit = macro foo_impl

  def bar_impl(c: Context)(x: c.Tree, xs: c.Tree*) = {
    import c.universe._
    q"println((${x.toString}, ${xs.toString}))"
  }

  def bar(x: Int = 1, xs: Int*): Unit = macro bar_impl

  def baz_impl(c: Context)(x: c.Tree, y: c.Tree, xs: c.Tree*) = {
    import c.universe._
    q"println((${x.toString}, ${y.toString}, ${xs.toString}))"
  }

  def baz(x: Int = 1, y: Int = 2, xs: Int*): Unit = macro baz_impl
}