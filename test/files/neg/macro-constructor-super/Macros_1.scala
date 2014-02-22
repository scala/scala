import scala.reflect.macros.Context
import scala.language.experimental.macros

object Macros {
  def ctor(c: Context)(x: c.Tree): c.Tree = {
    import c.universe._
    q"C($x)"
  }
}

class CState(val x: Int) extends AnyVal

class C private (state: CState) {
  def this(x: Int) = macro Macros.ctor
  def x = state.x
  override def toString = s"C($x)"
}