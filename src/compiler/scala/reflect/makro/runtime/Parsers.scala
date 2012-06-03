package scala.reflect.makro
package runtime

import language.existentials
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

trait Parsers {
  self: Context =>

  def parse(code: String): Tree =
    // todo. provide decent implementation
    try {
      import scala.reflect.runtime.{universe => ru}
      val parsed = ru.rootMirror.mkToolBox().parseExpr(code)
      val importer = universe.mkImporter(ru)
      importer.importTree(parsed)
    } catch {
      case ToolBoxError(msg, cause) =>
        throw new ParseError(universe.NoPosition, msg)
    }

  case class ParseError(val pos: Position, val msg: String) extends Throwable(msg)
  object ParseError extends ParseErrorExtractor
}