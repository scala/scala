/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interactive
package tests.core

import scala.reflect.internal.util.Position
import scala.reflect.internal.util.SourceFile

trait PresentationCompilerRequestsWorkingMode extends TestResources {

  protected def synchronousRequests: Boolean

  protected def askAllSources[T] = if (synchronousRequests) askAllSourcesSync[T] _ else askAllSourcesAsync[T] _

  /** Perform an operation on all sources at all positions that match the given
   *  `marker`. For instance, askAllSources(TypeMarker)(askTypeAt)(println) would
   *  ask the type at all positions marked with `TypeMarker.marker` and println the result.
   */
  private def askAllSourcesAsync[T](marker: TestMarker)(askAt: Position => Response[T])(f: (Position, T) => Unit): Unit = {
    val positions = allPositionsOf(str = marker.marker)
    val responses = for (pos <- positions) yield askAt(pos)

    for ((pos, r) <- positions zip responses) withResponse(pos, r)(f)
  }

  /** Synchronous version of askAllSources. Each position is treated in turn, waiting for the
   *  response before going to the next one.
   */
  private def askAllSourcesSync[T](marker: TestMarker)(askAt: Position => Response[T])(f: (Position, T) => Unit): Unit = {
    val positions = allPositionsOf(str = marker.marker)
    for (pos <- positions) withResponse(pos, askAt(pos))(f)
  }

  /** All positions of the given string in all source files. */
  private def allPositionsOf(srcs: Seq[SourceFile] = sourceFiles.toIndexedSeq, str: String): Seq[Position] =
    for (s <- srcs; p <- positionsOf(s, str)) yield p

  /** Return all positions of the given str in the given source file. */
  private def positionsOf(source: SourceFile, str: String): Seq[Position] = {
    val buf = new scala.collection.mutable.ListBuffer[Position]
    var pos = source.content.indexOfSlice(str)
    while (pos >= 0) {
      buf += source.position(pos - 1) // we need the position before the first character of this marker
      pos = source.content.indexOfSlice(str, pos + 1)
    }
    buf.toList
  }

  private def withResponse[T](pos: Position, response: Response[T])(f: (Position, T) => Unit): Unit = {
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
