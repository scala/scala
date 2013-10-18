import scala.reflect.macros.Context
import scala.language.experimental.macros

object Macros {
  def ctor(c: Context)(x: c.Tree): c.Tree = {
    import c.universe._
    q"C($x)"
  }
}