/* sbt -- Simple Build Tool
 * Copyright 2008, 2009, 2010  Mark Harrah
 */
package xsbt

	import xsbti.{F0,Logger,Maybe}
	import java.io.File

private object DelegatingReporter
{
	def apply(settings: scala.tools.nsc.Settings, delegate: xsbti.Reporter): DelegatingReporter =
		new DelegatingReporter(Command.getWarnFatal(settings), delegate)
}

private trait ReporterCompat27 {
	// this method is not in 2.7.7, so we need to have a dummy interface or scalac complains nothing is overridden
	def hasWarnings: Boolean
}
// The following code is based on scala.tools.nsc.reporters.{AbstractReporter, ConsoleReporter}
// Copyright 2002-2009 LAMP/EPFL
// Original author: Martin Odersky
private final class DelegatingReporter(warnFatal: Boolean, delegate: xsbti.Reporter) extends scala.tools.nsc.reporters.Reporter with ReporterCompat27
{
	import scala.tools.nsc.util.{FakePos,NoPosition,Position}

	def error(msg: String) { error(FakePos("scalac"), msg) }

	def printSummary() = delegate.printSummary()

		// this helps keep source compatibility with the changes in 2.8 : Position.{source,line,column} are no longer Option[X]s, just plain Xs
		// so, we normalize to Option[X]
	private def o[T](t: Option[T]): Option[T] = t
	private def o[T](t: T): Option[T] = Some(t)

	override def hasErrors = delegate.hasErrors
	override def hasWarnings = delegate.hasWarnings
	def problems = delegate.problems

	override def reset =
	{
		super.reset
		delegate.reset
	}
	protected def info0(pos: Position, msg: String, rawSeverity: Severity, force: Boolean)
	{
		val severity = if(warnFatal && rawSeverity == WARNING) ERROR else rawSeverity
		delegate.log(convert(pos), msg, convert(rawSeverity))
	}
	private[this] def convert(posIn: Position): xsbti.Position =
	{
		val pos =
			posIn match
			{
				case null | NoPosition => NoPosition
				case x: FakePos => x
				case x =>
					posIn.inUltimateSource(o(posIn.source).get)
			}
		pos match
		{
			case NoPosition | FakePos(_) => position(None, None, None, "", None, None, None)
			case _ => makePosition(pos)
		}
	}
	private[this] def makePosition(pos: Position): xsbti.Position =
	{
		val srcO = o(pos.source)
		val opt(sourcePath, sourceFile) = for(src <- srcO) yield (src.file.path, src.file.file)
		val line = o(pos.line)
		if(!line.isEmpty)
		{
			val lineContent = pos.lineContent.stripLineEnd
			val offsetO = o(pos.offset)
			val opt(pointer, pointerSpace) =
				for(offset <- offsetO; src <- srcO) yield
				{
					val pointer = offset - src.lineToOffset(src.offsetToLine(offset))
					val pointerSpace = ((lineContent: Seq[Char]).take(pointer).map { case '\t' => '\t'; case x => ' ' }).mkString
					(pointer, pointerSpace)
				}
			position(sourcePath, sourceFile, line, lineContent, offsetO, pointer, pointerSpace)
		}
		else
			position(sourcePath, sourceFile, line, "", None, None, None)
	}
	private[this] object opt
	{
		def unapply[A,B](o: Option[(A,B)]): Some[(Option[A], Option[B])] =
			Some(o match
			{
				case Some((a,b)) => (Some(a), Some(b))
				case None => (None, None)
			})
	}
	private[this] def position(sourcePath0: Option[String], sourceFile0: Option[File], line0: Option[Int], lineContent0: String, offset0: Option[Int], pointer0: Option[Int], pointerSpace0: Option[String]) =
		new xsbti.Position
		{
			val line = o2mi(line0)
			val lineContent = lineContent0
			val offset = o2mi(offset0)
			val sourcePath = o2m(sourcePath0)
			val sourceFile = o2m(sourceFile0)
			val pointer = o2mi(pointer0)
			val pointerSpace = o2m(pointerSpace0)
		}

		import xsbti.Severity.{Info, Warn, Error}
	private[this] def convert(sev: Severity): xsbti.Severity =
		sev match
		{
			case INFO => Info
			case WARNING => Warn
			case ERROR => Error
		}

		import java.lang.{Integer => I}
	private[this] def o2mi(opt: Option[Int]): Maybe[I] = opt match { case None => Maybe.nothing[I]; case Some(s) => Maybe.just[I](s) }
	private[this] def o2m[S](opt: Option[S]): Maybe[S] = opt match { case None => Maybe.nothing[S]; case Some(s) => Maybe.just(s) }
}