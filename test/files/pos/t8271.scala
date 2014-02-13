import scala.language.higherKinds
import scala.reflect.macros.blackbox.Context

object Macros {
  def foo(c: Context) = bar[List](c)
  def bar[M[X]](d: Context)(implicit tcon: d.WeakTypeTag[M[_]]) = ???
}
