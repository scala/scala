// scalac: -Xfatal-warnings
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macro {
  def blockToList[T](block: T): List[T] = macro impl[T]
  def impl[T: c.WeakTypeTag](c: Context)(block: c.Tree): c.Tree = {
    import c.universe._
    block match {
      case Block(stats, expr) =>
        q"_root_.scala.List.apply[${weakTypeOf[T]}](..${stats :+ expr})"
    }
  }
}
