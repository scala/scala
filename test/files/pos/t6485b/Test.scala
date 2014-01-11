import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

final class Ops[T](val x: T) extends AnyVal {
  def f = macro Macros.crash
}

object Macros {
  def crash(c: Context): c.Expr[Unit] = c.universe.reify(())
}