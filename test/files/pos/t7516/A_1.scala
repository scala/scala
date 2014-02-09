import scala.reflect._,macros._, scala.language.experimental.macros

object A {
  def impl[T: c.WeakTypeTag](c: Context)(t: c.Expr[T]): c.Expr[List[T]] = {
    val r = c.universe.reify { List(t.splice) }
    c.Expr[List[T]]( c.untypecheck(r.tree) )
  }
  def demo[T](t: T): List[T] = macro impl[T]
}
