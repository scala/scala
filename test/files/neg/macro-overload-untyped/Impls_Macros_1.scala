import scala.reflect.macros.Context

object Macros {
  def impl(c: Context)(x: c.Tree, y: c.Tree) = x

  def notOk1(x: _, y: String) = macro impl
  def notOk1(x: Int, y: Int) = macro impl

  def notOk2(x: _, y: String) = macro impl
  def notOk2(x: _, y: Int) = macro impl

  def notOk3(x: _, y: String) = macro impl
  def notOk3(x: Int, y: Int) = ???

  def ok1(x: String, y: String): _ = macro impl
  def ok1(x: Int, y: Int): _ = macro impl

  def ok2(x: String, y: String): _ = macro impl
  def ok2(x: Int, y: Int): Int = macro impl

  def ok3(x: String, y: String): _ = macro impl
  def ok3(x: Int, y: Int): Int = ???

  type NotOk4(x: _, y: String) = macro impl
  type NotOk4(x: Int, y: Int) = macro impl

  type NotOk5(x: _, y: String) = macro impl
  type NotOk5(x: _, y: Int) = macro impl
}