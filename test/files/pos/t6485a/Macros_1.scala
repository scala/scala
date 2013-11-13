import scala.reflect.macros.BlackboxContext

object Macros {
  def crash(c: BlackboxContext): c.Expr[Unit] = c.universe.reify(())
}