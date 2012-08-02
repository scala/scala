package scala.reflect.macros
package runtime

trait ExprUtils {
  self: Context =>

  import universe._
  import mirror._

  def literalNull = Expr[Null](Literal(Constant(null)))(TypeTag.Null)

  def literalUnit = Expr[Unit](Literal(Constant(())))(TypeTag.Unit)

  def literalTrue = Expr[Boolean](Literal(Constant(true)))(TypeTag.Boolean)

  def literalFalse = Expr[Boolean](Literal(Constant(false)))(TypeTag.Boolean)

  def literal(x: Boolean) = Expr[Boolean](Literal(Constant(x)))(TypeTag.Boolean)

  def literal(x: Byte) = Expr[Byte](Literal(Constant(x)))(TypeTag.Byte)

  def literal(x: Short) = Expr[Short](Literal(Constant(x)))(TypeTag.Short)

  def literal(x: Int) = Expr[Int](Literal(Constant(x)))(TypeTag.Int)

  def literal(x: Long) = Expr[Long](Literal(Constant(x)))(TypeTag.Long)

  def literal(x: Float) = Expr[Float](Literal(Constant(x)))(TypeTag.Float)

  def literal(x: Double) = Expr[Double](Literal(Constant(x)))(TypeTag.Double)

  def literal(x: String) = Expr[String](Literal(Constant(x)))(TypeTag[String](definitions.StringClass.toTypeConstructor))

  def literal(x: Char) = Expr[Char](Literal(Constant(x)))(TypeTag.Char)
}
