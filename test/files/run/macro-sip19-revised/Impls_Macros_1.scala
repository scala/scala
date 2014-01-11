import scala.reflect.macros.whitebox.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._

    val inscope = c.inferImplicitValue(c.mirror.staticClass("SourceLocation").toType)
    val outer = c.Expr[SourceLocation](if (!inscope.isEmpty) inscope else Literal(Constant(null)))

    val Apply(fun, args) = c.enclosingImplicits(0).tree
    val fileName = fun.pos.source.file.file.getName
    val line = fun.pos.line
    val charOffset = fun.pos.point
    def literal[T](x: T) = c.Expr[T](Literal(Constant(x)))
    c.universe.reify { SourceLocation1(outer.splice, literal(fileName).splice, literal(line).splice, literal(charOffset).splice) }
  }

  implicit def sourceLocation: SourceLocation1 = macro impl
}

trait SourceLocation {
  /** Source location of the outermost call */
  val outer: SourceLocation

  /** The name of the source file */
  val fileName: String

  /** The line number */
  val line: Int

  /** The character offset */
  val charOffset: Int
}

case class SourceLocation1(val outer: SourceLocation, val fileName: String, val line: Int, val charOffset: Int) extends SourceLocation