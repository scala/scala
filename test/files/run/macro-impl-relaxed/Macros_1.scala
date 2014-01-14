import language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  def implUU(c: Context)(x: c.Tree): c.Tree = x
  def implTU(c: Context)(x: c.Expr[Int]): c.Tree = x.tree
  def implUT(c: Context)(x: c.Tree): c.Expr[Int] = c.Expr[Int](x)
  def implTT(c: Context)(x: c.Expr[Int]): c.Expr[Int] = x

  def fooUU(x: Int): Int = macro implUU
  def fooTU(x: Int): Int = macro implTU
  def fooUT(x: Int): Int = macro implUT
  def fooTT(x: Int): Int = macro implTT
}