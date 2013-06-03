package scala.reflect.macros
package contexts

import scala.language.existentials
import scala.tools.reflect.ToolBox
import scala.tools.reflect.ToolBoxError

trait Parsers {
  self: Context =>

  def parse(code: String): Tree =
    // todo. provide decent implementation
    // see `Typers.typedUseCase` for details
    try {
      import scala.reflect.runtime.{universe => ru}
      val parsed = ru.rootMirror.mkToolBox().parse(code)
      val importer = universe.mkImporter(ru)
      importer.importTree(parsed)
    } catch {
      case ToolBoxError(msg, cause) =>
        // todo. provide a position
        throw new ParseException(universe.NoPosition, msg)
    }
}
