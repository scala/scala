/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import java.io.File
import java.util.Optional

import scala.reflect.internal.util.{ FakePos, NoPosition, Position }
// Left for compatibility
import Compat._

private object DelegatingReporter {
  def apply(settings: scala.tools.nsc.Settings, delegate: xsbti.Reporter): DelegatingReporter =
    new DelegatingReporter(Command.getWarnFatal(settings), Command.getNoWarn(settings), delegate)

  class PositionImpl(
      sourcePath0: Option[String],
      sourceFile0: Option[File],
      line0: Option[Int],
      lineContent0: String,
      offset0: Option[Int],
      pointer0: Option[Int],
      pointerSpace0: Option[String],
      startOffset0: Option[Int],
      endOffset0: Option[Int],
      startLine0: Option[Int],
      startColumn0: Option[Int],
      endLine0: Option[Int],
      endColumn0: Option[Int]
  ) extends xsbti.Position {
    val line = o2oi(line0)
    val lineContent = lineContent0
    val offset = o2oi(offset0)
    val sourcePath = o2jo(sourcePath0)
    val sourceFile = o2jo(sourceFile0)
    val pointer = o2oi(pointer0)
    val pointerSpace = o2jo(pointerSpace0)
    override val startOffset = o2oi(startOffset0)
    override val endOffset = o2oi(endOffset0)
    override val startLine = o2oi(startLine0)
    override val startColumn = o2oi(startColumn0)
    override val endLine = o2oi(endLine0)
    override val endColumn = o2oi(endColumn0)
    override def toString =
      (sourcePath0, line0) match {
        case (Some(s), Some(l)) => s + ":" + l
        case (Some(s), _)       => s + ":"
        case _                  => ""
      }
  }

  object PositionImpl {
    def empty: PositionImpl =
      new PositionImpl(None, None, None, "", None, None, None, None, None, None, None, None, None)
  }

  import java.lang.{ Integer => I }
  private[xsbt] def o2oi(opt: Option[Int]): Optional[I] = {
    opt match {
      case Some(s) => Optional.ofNullable[I](s: I)
      case None    => Optional.empty[I]
    }
  }

  private[xsbt] def o2jo[A](o: Option[A]): Optional[A] = {
    o match {
      case Some(v) => Optional.ofNullable(v)
      case None    => Optional.empty[A]()
    }
  }

  private[xsbt] def convert(dirtyPos: Position): xsbti.Position = {
    def cleanPos(pos: Position) = {
      Option(pos) match {
        case None | Some(NoPosition) => None
        case Some(_: FakePos)        => None
        case _                       => Option(pos.finalPosition)
      }
    }

    def makePosition(pos: Position): xsbti.Position = {
      val src = pos.source
      val sourcePath = src.file.path
      val sourceFile = src.file.file
      val line = pos.line
      val lineContent = pos.lineContent.stripLineEnd
      val offset = pos.point

      // Same logic as Position#line
      def lineOf(offset: Int) = src.offsetToLine(offset) + 1
      def columnOf(offset: Int) = offset - src.lineToOffset(src.offsetToLine(offset))

      val pointer = columnOf(offset)
      val pointerSpace = lineContent.toList.take(pointer).map {
        case '\t' => '\t'
        case _    => ' '
      }

      val startOffset = if (pos.isRange) Some(pos.start) else None
      val endOffset = if (pos.isRange) Some(pos.end) else None
      val startLine = if (pos.isRange) Some(lineOf(pos.start)) else None
      val startColumn = if (pos.isRange) Some(columnOf(pos.start)) else None
      val endLine =
        if (pos.isRange)
          try {
            Some(lineOf(pos.end))
          } catch {
            // work around for https://github.com/scala/bug/issues/11865 by falling back to start pos
            case _: ArrayIndexOutOfBoundsException =>
              startLine
          } else None
      val endColumn =
        if (pos.isRange)
          try {
            Some(columnOf(pos.end))
          } catch {
            // work around for https://github.com/scala/bug/issues/11865 by falling back to start pos
            case _: ArrayIndexOutOfBoundsException =>
              startColumn
          } else None

      new PositionImpl(
        Option(sourcePath),
        Option(sourceFile),
        Option(line),
        lineContent,
        Option(offset),
        Option(pointer),
        Option(pointerSpace.mkString),
        startOffset,
        endOffset,
        startLine,
        startColumn,
        endLine,
        endColumn
      )
    }

    cleanPos(dirtyPos) match {
      case None           => PositionImpl.empty
      case Some(cleanPos) => makePosition(cleanPos)
    }
  }
}

// Copyright 2002-2009 LAMP/EPFL
// Original author: Martin Odersky
// Based on scala.tools.nsc.reporters.{AbstractReporter, ConsoleReporter}
private final class DelegatingReporter(
    warnFatal: Boolean,
    noWarn: Boolean,
    private[this] var delegate: xsbti.Reporter
) extends scala.tools.nsc.reporters.Reporter {
  def dropDelegate(): Unit = { delegate = null }
  def error(msg: String): Unit = error(FakePos("scalac"), msg)
  def printSummary(): Unit = delegate.printSummary()

  def problems = delegate.problems
  override def hasErrors = delegate.hasErrors
  override def hasWarnings = delegate.hasWarnings
  override def comment(pos: Position, msg: String): Unit =
    delegate.comment(DelegatingReporter.convert(pos), msg)
  override def reset(): Unit = {
    super.reset()
    delegate.reset()
  }

  protected def info0(pos: Position, msg: String, rawSeverity: Severity, force: Boolean): Unit = {
    val skip = rawSeverity == WARNING && noWarn
    if (!skip) {
      val severity = if (warnFatal && rawSeverity == WARNING) ERROR else rawSeverity
      delegate.log(new CompileProblem(DelegatingReporter.convert(pos), msg, convert(severity)))
    }
  }

  import xsbti.Severity.{ Info, Warn, Error }
  private[this] def convert(sev: Severity): xsbti.Severity = {
    sev match {
      case INFO    => Info
      case WARNING => Warn
      case ERROR   => Error
    }
  }

  // Define our own problem because the bridge should not depend on sbt util-logging.
  import xsbti.{ Problem => XProblem, Position => XPosition, Severity => XSeverity }
  private final class CompileProblem(
      pos: XPosition,
      msg: String,
      sev: XSeverity
  ) extends XProblem {
    override val category = ""
    override val position = pos
    override val message = msg
    override val severity = sev
    override def toString = s"[$severity] $pos: $message"
  }
}
