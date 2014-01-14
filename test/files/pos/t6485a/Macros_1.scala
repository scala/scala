import scala.reflect.macros.blackbox.Context

object Macros {
  def crash(c: Context): c.Expr[Unit] = c.universe.reify(())
}