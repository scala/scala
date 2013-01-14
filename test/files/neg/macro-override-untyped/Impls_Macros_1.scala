import scala.reflect.macros.Context

object Impls {
  def impl(c: Context)(x: c.Tree): c.Expr[Int] = c.Expr(x)
  def timpl(c: Context)(x: c.Tree) = x
}
import Impls._

trait BaseMacros {
  def untyped(x: _) = macro impl
  def kindaUntyped(x: Int): _ = macro impl
  def typed(x: Int) = macro impl

  type Untyped(x: _) = macro timpl
  type Typed(x: Int) = macro timpl
}

trait Macros extends BaseMacros {
  override def untyped(x: Int) = macro impl
  override def kindaUntyped(x: Int): Int = macro impl // ok!
  override def typed(x: _) = macro impl

  override type Untyped(x: Int) = macro timpl
  override type Typed(x: _) = macro timpl
}