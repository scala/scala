package scala.reflect
package macros

/** A slice of [[scala.reflect.macros.Context the Scala macros context]] that
 *  exposes functions to parse strings with Scala code into trees.
 */
trait Parsers {
  self: Context =>

  /** Parses a string with a Scala expression into an abstract syntax tree.
   *  Only works for expressions, i.e. parsing a package declaration will fail.
   *  @throws [[scala.reflect.macros.ParseException]]
   */
  def parse(code: String): Tree
}

/** Indicates an error during [[scala.reflect.macros.Parsers#parse]].
 */
case class ParseException(val pos: scala.reflect.api.Position, val msg: String) extends Exception(msg)
