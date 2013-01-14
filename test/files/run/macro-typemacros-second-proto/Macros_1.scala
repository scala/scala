import scala.reflect.macros.Context
import language.experimental.macros

object Macros {
  def impl[T: c.WeakTypeTag](c: Context)(name: c.Expr[String]) = c.universe.TypeTree(c.weakTypeOf[T])
  def impl2(c: Context) = c.universe.TypeTree(c.typeOf[Int])
}
