package scala.reflect
package makro

trait Parsers {
  self: Context =>

  /** .. */
  // todo. distinguish between `parseExpr` and `parse`
  def parse(code: String): Tree

  /** Represents an error during parsing
   */
  type ParseError <: Throwable
  val ParseError: ParseErrorExtractor
  abstract class ParseErrorExtractor {
    def unapply(error: ParseError): Option[(Position, String)]
  }
}