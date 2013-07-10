import language.experimental.macros
import scala.reflect.macros.Macro

trait Impls extends Macro {
  import c.universe._
  def impl1(x: Expr[Int]) = q"println(x)"
  def impl2(x: Tree) = q"println(x)"
  def impl3(x: Block) = q"println(x)"
}

object Macros {
  def m1(x: Int) = macro Impls.impl1
  def m2(x: Int) = macro Impls.impl2
  def m3(x: Int) = macro Impls.impl3
}