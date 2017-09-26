import language.experimental.macros
import scala.reflect.macros.blackbox.Context

// wraps a new Block so typer sees a local import on second typecheck
//
object Macro {
  def apply(a: Any): Any = macro impl

  def impl(c: Context)(a: c.Tree): c.Tree = {
    import c.universe._
    a match {
      case Block(stmts, res) => Block(stmts, res)
      case expr              => Block(Nil, expr)
    }
  }
}
