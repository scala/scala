import scala.reflect.macros.Context

object Macros {
  def impl1(c: Context)(x: c.Tree, y: c.Tree) = x
  def unsound1(x: _, y: Int) = macro impl1

  def impl2(c: Context)(x: c.Tree, y: c.Expr[Int]) = x
  def unsound2(x: _, y: _) = macro impl2

  def impl3(c: Context)(x: c.Tree, y: c.Tree) = x
  def unsound3[T <: Int](x: _, y: T) = macro impl3

  def impl4(c: Context)(x: c.Tree, y: c.Tree, z: c.Tree) = x
  def unsound4[T, U <: T](x: _, y: T, z: U) = macro impl4

  def impl5(c: Context)(x: c.Tree, y: c.Tree) = x
  def unsound5[M[_]](x: _, y: M[_]) = macro impl5
}