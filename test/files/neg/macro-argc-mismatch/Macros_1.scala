import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def one(x: Int): Unit = macro oneImpl
  def oneImpl(c: Context)(x: c.Tree) = {
    import c.universe._
    q"()"
  }

  def two(x: Int)(y: Int): Unit = macro twoImpl
  def twoImpl(c: Context)(x: c.Tree)(y: c.Tree) = {
    import c.universe._
    q"()"
  }
}
