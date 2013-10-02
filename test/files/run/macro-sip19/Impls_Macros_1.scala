import scala.reflect.macros.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val Apply(fun, args) = c.enclosingImplicits(0).tree
    val fileName = fun.pos.source.file.file.getName
    val line = fun.pos.line
    val charOffset = fun.pos.point
    c.universe.reify { SourceLocation(c.literal(fileName).splice, c.literal(line).splice, c.literal(charOffset).splice) }
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