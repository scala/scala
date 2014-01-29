import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Macros {
  implicit class Error(ctx: StringContext) {
    def err(args: Any*): Unit = macro impl
  }

  def impl(c: Context)(args: c.Tree*): c.Tree = {
    import c.universe._
    val q"Macros.Error(scala.StringContext.apply($arg)).err()" = c.macroApplication
    for (i <- 1 to 3) c.error(arg.pos.withPoint(arg.pos.point + i - 1), i.toString)
    q"()"
  }
}