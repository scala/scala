package scala.reflect
package makro

trait ExprUtils {
  self: Context =>

  def literalNull: Expr[Null]

  def literalUnit: Expr[Unit]

  def literalTrue: Expr[Boolean]

  def literalFalse: Expr[Boolean]

  def literal(x: Boolean): Expr[Boolean]

  def literal(x: Byte): Expr[Byte]

  def literal(x: Short): Expr[Short]

  def literal(x: Int): Expr[Int]

  def literal(x: Long): Expr[Long]

  def literal(x: Float): Expr[Float]

  def literal(x: Double): Expr[Double]

  def literal(x: String): Expr[String]

  def literal(x: Char): Expr[Char]
}
