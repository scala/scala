/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import java.io.File
import java.{ util => ju }
import ju.Optional

import scala.jdk.CollectionConverters._
import scala.reflect.internal.util.{ CodeAction, FakePos, NoPosition, Position }
import scala.reflect.io.AbstractFile
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.FilteringReporter
import xsbti.{
  Action,
  DiagnosticCode => XDiagnosticCode,
  DiagnosticRelatedInformation => XDiagnosticRelatedInformation,
  Problem => XProblem,
  Position => XPosition,
  Severity => XSeverity,
  TextEdit,
  WorkspaceEdit
}

/**
 * This implements reporter/ concrete Problem data structure for
 * the compiler bridge, in other words for each Scala versions
 * that Zinc is capable of compiling.
 *
 * There's also sbt.util.InterfaceUtil, which is also used in
 * Zinc in the Scala version Zinc uses.
 */
private object DelegatingReporter {
  def apply(settings: Settings, delegate: xsbti.Reporter): DelegatingReporter =
    new DelegatingReporter(settings, delegate)

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

  private[xsbt] def l2jl[A](l: List[A]): ju.List[A] = {
    val jl = new ju.ArrayList[A](l.size)
    l.foreach(jl.add(_))
    jl
  }

  private[xsbt] def jl2l[A](jl: ju.List[A]): List[A] = {
    jl.asScala.toList
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
      val sourcePath = src.file match {
        case AbstractZincFile(virtualFile) => virtualFile.id
        case af: AbstractFile              => af.path
      }
      val sourceFile = new File(src.file.path)
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
          }
        else None
      val endColumn =
        if (pos.isRange)
          try {
            Some(columnOf(pos.end))
          } catch {
            // work around for https://github.com/scala/bug/issues/11865 by falling back to start pos
            case _: ArrayIndexOutOfBoundsException =>
              startColumn
          }
        else None

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
    val settings: Settings,
    private[this] var delegate: xsbti.Reporter
) extends FilteringReporter {
  import DelegatingReporter._

  private val Werror: Boolean = settings.fatalWarnings.value
  private val noWarn: Boolean = settings.nowarn.value

  def dropDelegate(): Unit = { delegate = ReporterSink }
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

  override def doReport(pos: Position, msg: String, rawSeverity: Severity, actions: List[CodeAction]): Unit = {
    val skip = rawSeverity == WARNING && noWarn
    if (!skip) {
      val severity = if (Werror && rawSeverity == WARNING) ERROR else rawSeverity
      val pos1 = DelegatingReporter.convert(pos)
      delegate.log(new CompileProblem(
        pos = pos1,
        msg = msg,
        sev = convert(severity),
        rendered0 = None,
        diagnosticCode0 = None,
        diagnosticRelatedInformation0 = Nil,
        actions0 = actions.map(convertAction),
      ))
    }
  }

  //protected def info0(pos: Position, msg: String, rawSeverity: Severity, force: Boolean): Unit = doReport(pos, msg, rawSeverity, Nil)

  import xsbti.Severity.{ Info, Warn, Error }
  private[this] def convert(sev: Severity): xsbti.Severity = sev match {
    case INFO    => Info
    case WARNING => Warn
    case ERROR   => Error
    case x       => throw new MatchError(x)
  }

  // Define our own problem because the bridge should not depend on sbt util-logging.
  private final class CompileProblem(
      pos: XPosition,
      msg: String,
      sev: XSeverity,
      rendered0: Option[String],
      diagnosticCode0: Option[XDiagnosticCode],
      diagnosticRelatedInformation0: List[XDiagnosticRelatedInformation],
      actions0: List[Action]
  ) extends XProblem {
    override val category = ""
    override val position = pos
    override val message = msg
    override val severity = sev
    override def rendered = o2jo(rendered0)
    override def toString = s"[$severity] $pos: $message"
    override def diagnosticCode: Optional[XDiagnosticCode] = o2jo(diagnosticCode0)
    override def diagnosticRelatedInformation(): ju.List[XDiagnosticRelatedInformation] =
      l2jl(diagnosticRelatedInformation0)
    override def actions(): ju.List[Action] = l2jl(actions0)
  }

  private def convertAction(a: CodeAction): Action =
    action(
      title = a.title,
      description = a.description,
      edit = workspaceEdit(a.edits.map { edit =>
        textEdit(DelegatingReporter.convert(edit.position), edit.newText)
      }),
    )

  private def action(
      title: String,
      description: Option[String],
      edit: WorkspaceEdit
  ): Action =
    new ConcreteAction(title, description, edit)

  private def workspaceEdit(changes: List[TextEdit]): WorkspaceEdit =
    new ConcreteWorkspaceEdit(changes)

  private def textEdit(position: XPosition, newText: String): TextEdit =
    new ConcreteTextEdit(position, newText)

  private final class ConcreteAction(
      title0: String,
      description0: Option[String],
      edit0: WorkspaceEdit
  ) extends Action {
    val title: String = title0
    val edit: WorkspaceEdit = edit0
    override def description(): Optional[String] =
      o2jo(description0)
    override def toString(): String =
      s"Action($title0, $description0, $edit0)"
    private def toTuple(a: Action) =
      (
        a.title,
        a.description,
        a.edit
      )
    override def hashCode: Int = toTuple(this).##
    override def equals(o: Any): Boolean = o match {
      case o: Action => toTuple(this) == toTuple(o)
      case _         => false
    }
  }

  private final class ConcreteWorkspaceEdit(changes0: List[TextEdit]) extends WorkspaceEdit {
    override def changes(): ju.List[TextEdit] = l2jl(changes0)
    override def toString(): String =
      s"WorkspaceEdit($changes0)"
    private def toTuple(w: WorkspaceEdit) = jl2l(w.changes)
    override def hashCode: Int = toTuple(this).##
    override def equals(o: Any): Boolean = o match {
      case o: WorkspaceEdit => toTuple(this) == toTuple(o)
      case _                => false
    }
  }

  private final class ConcreteTextEdit(position0: XPosition, newText0: String) extends TextEdit {
    val position: XPosition = position0
    val newText: String = newText0
    override def toString(): String =
      s"TextEdit($position, $newText)"
    private def toTuple(edit: TextEdit) =
      (
        edit.position,
        edit.newText
      )
    override def hashCode: Int = toTuple(this).##
    override def equals(o: Any): Boolean = o match {
      case o: TextEdit => toTuple(this) == toTuple(o)
      case _           => false
    }
  }
}

private object ReporterSink extends xsbti.Reporter {
  def reset() = ()
  def hasErrors() = false
  def hasWarnings() = false
  def printSummary() = ()
  def problems() = Array.empty[xsbti.Problem]
  def log(problem: xsbti.Problem) = ()
  def comment(pos: xsbti.Position, msg: String) = ()
}
