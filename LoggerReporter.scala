/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{F0,Logger}

private object LoggerReporter
{
	def apply(settings: scala.tools.nsc.Settings, maximumErrors: Int, log: Logger): LoggerReporter =
		new LoggerReporter(Command.getWarnFatal(settings), maximumErrors, log)
}

// The following code is based on scala.tools.nsc.reporters.{AbstractReporter, ConsoleReporter}
// Copyright 2002-2009 LAMP/EPFL
// Original author: Martin Odersky
private final class LoggerReporter(warnFatal: Boolean, maximumErrors: Int, log: Logger) extends scala.tools.nsc.reporters.Reporter
{
	import scala.tools.nsc.util.{FakePos,NoPosition,Position}
	private val positions = new scala.collection.mutable.HashMap[Position, Severity]

	def error(msg: String) { error(FakePos("scalac"), msg) }

	def printSummary()
	{
		if(WARNING.count > 0)
			log.warn(Message(countElementsAsString(WARNING.count, "warning") + " found"))
		if(ERROR.count > 0)
			log.error(Message(countElementsAsString(ERROR.count, "error") + " found"))
	}

	def display(pos: Position, msg: String, severity: Severity)
	{
		severity.count += 1
		if(severity != ERROR || maximumErrors < 0 || severity.count <= maximumErrors)
			print(severityLogger(severity), pos, msg)
	}
	private def severityLogger(severity: Severity) =
		(m: F0[String]) =>
		{
			(severity match
			{
				case ERROR => log.error(m)
				case WARNING => log.warn(m)
				case INFO => log.info(m)
			})
		}

		// this helps keep source compatibility with the changes in 2.8 : Position.{source,line,column} are no longer Option[X]s, just plain Xs
		// so, we normalize to Option[X]
	private def o[T](t: Option[T]): Option[T] = t
	private def o[T](t: T): Option[T] = Some(t)
	private def print(logger: F0[String] => Unit, posIn: Position, msg: String)
	{
		def log(s: => String) = logger(Message(s))
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
			case NoPosition => log(msg)
			case FakePos(fmsg) => log(fmsg+" "+msg)
			case _ =>
				val sourcePrefix = o(pos.source).map(_.file.path).getOrElse("")
				val lineNumberString = o(pos.line).map(line => ":" + line + ":").getOrElse(":") + " "
				log(sourcePrefix + lineNumberString + msg)
				if (!o(pos.line).isEmpty)
				{
					val lineContent = pos.lineContent.stripLineEnd
					log(lineContent) // source line with error/warning
					for(offset <- o(pos.offset); src <- o(pos.source))
					{
						val pointer = offset - src.lineToOffset(src.offsetToLine(offset))
						val pointerSpace = (lineContent: Seq[Char]).take(pointer).map { case '\t' => '\t'; case x => ' ' }
						log(pointerSpace.mkString + "^") // pointer to the column position of the error/warning
					}
				}
		}
	}
	override def reset =
	{
		super.reset
		positions.clear
	}

	
	protected def info0(pos: Position, msg: String, rawSeverity: Severity, force: Boolean)
	{
		val severity = if(warnFatal && rawSeverity == WARNING) ERROR else rawSeverity
		severity match
		{
			case WARNING | ERROR =>
			{
				if(!testAndLog(pos, severity))
					display(pos, msg, severity)
			}
			case _ => display(pos, msg, severity)
		}
	}

	private def testAndLog(pos: Position, severity: Severity): Boolean =
	{
		if(pos == null || pos.offset.isEmpty)
			false
		else if(positions.get(pos).map(_ >= severity).getOrElse(false))
			true
		else
		{
			positions(pos) = severity
			false
		}
	}
}