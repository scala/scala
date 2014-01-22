import scala.reflect.macros.whitebox._
import scala.language.experimental.macros

object Macros {
  def impl(c: Context)(x: c.Tree, y: c.Tree) = {
    import c.universe._
    q"println($x)"
  }

  def foo(x: Int, y: Int): Unit = macro impl
}