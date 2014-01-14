import scala.reflect.macros.whitebox.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val Apply(fun, args) = c.enclosingImplicits(0).tree
    val fileName = fun.pos.source.file.file.getName
    val line = fun.pos.line
    val charOffset = fun.pos.point
    def literal[T](x: T) = c.Expr[T](Literal(Constant(x)))
    c.universe.reify { SourceLocation(literal(fileName).splice, literal(line).splice, literal(charOffset).splice) }
  }

  implicit def sourceLocation: SourceLocation = macro impl
}

case class SourceLocation(
  /** The name of the source file */
  val fileName: String,

  /** The line number */
  val line: Int,

  /** The character offset */
  val charOffset: Int
)