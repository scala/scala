package scala.reflect
package macros

trait Parsers {
  self: Context =>

  /** .. */
  // todo. distinguish between parsing an expression and parsing arbitrary code
  // for example, parsing in expression mode will fail on packages
  def parse(code: String): Tree
}

/** Indicates an error during [[scala.reflect.macros.Parsers#Parse]].
 */
case class ParseException(val pos: scala.reflect.api.Position, val msg: String) extends Exception(msg)
