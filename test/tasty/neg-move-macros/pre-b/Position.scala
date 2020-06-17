package tastytest

import scala.reflect.macros.blackbox.Context

final case class Position(sourceName: String, line: Int)

object Position {
  def posImpl(c: Context): c.Expr[Position] = {
    import c.universe._
    val fileName = c.enclosingPosition.source.path.split('/').last
    val line = c.enclosingPosition.line
    c.Expr(q"new Position($fileName, $line)")
  }
}
