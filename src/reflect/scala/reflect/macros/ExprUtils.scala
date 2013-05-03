package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * A slice of [[scala.reflect.macros.Context the Scala macros context]] that defines shorthands for the
 *  most common `Expr`-creating functions.
 */
trait ExprUtils {
  self: Context =>

  /** Shorthand for `Literal(Constant(null))` in the underlying `universe`. */
  def literalNull: Expr[Null]

  /** Shorthand for `Literal(Constant(()))` in the underlying `universe`. */
  def literalUnit: Expr[Unit]

  /** Shorthand for `Literal(Constant(true))` in the underlying `universe`. */
  def literalTrue: Expr[Boolean]

  /** Shorthand for `Literal(Constant(false))` in the underlying `universe`. */
  def literalFalse: Expr[Boolean]

  /** Shorthand for `Literal(Constant(x: Boolean))` in the underlying `universe`. */
  def literal(x: Boolean): Expr[Boolean]

  /** Shorthand for `Literal(Constant(x: Byte))` in the underlying `universe`. */
  def literal(x: Byte): Expr[Byte]

  /** Shorthand for `Literal(Constant(x: Short))` in the underlying `universe`. */
  def literal(x: Short): Expr[Short]

  /** Shorthand for `Literal(Constant(x: Int))` in the underlying `universe`. */
  def literal(x: Int): Expr[Int]

  /** Shorthand for `Literal(Constant(x: Long))` in the underlying `universe`. */
  def literal(x: Long): Expr[Long]

  /** Shorthand for `Literal(Constant(x: Float))` in the underlying `universe`. */
  def literal(x: Float): Expr[Float]

  /** Shorthand for `Literal(Constant(x: Double))` in the underlying `universe`. */
  def literal(x: Double): Expr[Double]

  /** Shorthand for `Literal(Constant(x: String))` in the underlying `universe`. */
  def literal(x: String): Expr[String]

  /** Shorthand for `Literal(Constant(x: Char))` in the underlying `universe`. */
  def literal(x: Char): Expr[Char]
}
