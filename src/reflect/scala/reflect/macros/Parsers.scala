package scala.reflect
package macros

trait Parsers {
  self: Context =>

  /** .. */
  // todo. distinguish between parsing an expression and parsing arbitrary code
  // for example, parsing in expression mode will fail on packages
  def parse(code: String): Tree
}

// should be path-dependent, otherwise exception handling becomes a mess
case class ParseError(val pos: scala.reflect.api.Position, val msg: String) extends Throwable(msg)
