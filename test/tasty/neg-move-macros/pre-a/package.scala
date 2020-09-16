import scala.util.Random

import scala.reflect.macros.blackbox.Context

package object tastytest {

  object Macros {
    def posImpl(c: Context): c.Expr[Position] = {
      import c.universe._
      val fileName = c.enclosingPosition.source.path.split('/').last
      val line = c.enclosingPosition.line
      c.Expr(q"new Position($fileName, $line)")
    }
  }

}
