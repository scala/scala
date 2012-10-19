import scala.reflect.macros.Context

object Macros {
  def crash(c: Context): c.Expr[Unit] = c.universe.reify(())
}