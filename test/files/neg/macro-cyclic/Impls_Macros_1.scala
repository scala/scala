import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    c.universe.reify { implicitly[SourceLocation] }
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
