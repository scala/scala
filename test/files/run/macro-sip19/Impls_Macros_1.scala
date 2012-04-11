import scala.reflect.makro.Context

object Macros {
  def impl(c: Context) = {
    import c.mirror._
    val Apply(fun, args) = c.enclosingImplicits(0)._2
    val fileName = fun.pos.fileInfo.getName
    val line = fun.pos.line
    val charOffset = fun.pos.point
    c.reify { SourceLocation(c.literal(fileName).eval, c.literal(line).eval, c.literal(charOffset).eval) }
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
