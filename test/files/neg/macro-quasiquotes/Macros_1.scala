import language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Impls(val c: Context) {
  import c.universe._
  def impl1(x: Expr[Int]) = q"println(x)"
  def impl2(x: Tree) = q"println(x)"
  def impl3(x: Block) = q"println(x)"
}

object Macros {
  def m1(x: Int): Unit = macro Impls.impl1
  def m2(x: Int): Unit = macro Impls.impl2
  def m3(x: Int): Unit = macro Impls.impl3
}