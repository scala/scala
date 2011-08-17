package scala.tools.nsc
package interactive
package tests.core

import scala.tools.nsc.util.Position
import scala.tools.nsc.util.SourceFile

trait PresentationCompilerRequestsWorkingMode extends TestResources {

  protected def synchronousRequests: Boolean

  protected def askAllSources[T] = if (synchronousRequests) askAllSourcesSync[T] _ else askAllSourcesAsync[T] _

  /** Perform an operation on all sources at all positions that match the given
   *  `marker`. For instance, askAllSources(TypeMarker)(askTypeAt)(println) would
   *  ask the type at all positions marked with `TypeMarker.marker` and println the result.
   */
  private def askAllSourcesAsync[T](marker: TestMarker)(askAt: Position => Response[T])(f: (Position, T) => Unit) {
    val positions = allPositionsOf(marker.marker).valuesIterator.toList.flatten
    val responses = for (pos <- positions) yield askAt(pos)

    for ((pos, r) <- positions zip responses) withResponse(pos, r)(f)
  }

  /** Synchronous version of askAllSources. Each position is treated in turn, waiting for the
   *  response before going to the next one.
   */
  private def askAllSourcesSync[T](marker: TestMarker)(askAt: Position => Response[T])(f: (Position, T) => Unit) {
    val positions = allPositionsOf(marker.marker).valuesIterator.toList.flatten
    for (pos <- positions) withResponse(pos, askAt(pos))(f)
  }

  private def allPositionsOf: String => Map[SourceFile, Seq[Position]] =
    FindOccurrences(sourceFiles) _

  private def withResponse[T](pos: Position, response: Response[T])(f: (Position, T) => Unit) {
    /** Return the filename:line:col version of this position. */
    def showPos(pos: Position): String =
      "%s:%d:%d".format(pos.source.file.name, pos.line, pos.column)

    response.get(TIMEOUT) match {
      case Some(Left(t)) =>
        f(pos, t)
      case None =>
        println("TIMEOUT: " + showPos(pos))
      case Some(r) =>
        println("ERROR: " + r)
    }
  }
}