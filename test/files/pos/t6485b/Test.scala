import scala.language.experimental.macros
import scala.reflect.macros.BlackboxContext

final class Ops[T](val x: T) extends AnyVal {
  def f = macro Macros.crash
}

object Macros {
  def crash(c: BlackboxContext): c.Expr[Unit] = c.universe.reify(())
}