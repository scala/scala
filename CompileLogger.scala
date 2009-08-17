/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{F0,Logger}

// The following code is based on scala.tools.nsc.reporters.{AbstractReporter, ConsoleReporter}
// Copyright 2002-2009 LAMP/EPFL
// Original author: Martin Odersky
private final class LoggerReporter(maximumErrors: Int, log: Logger) extends scala.tools.nsc.reporters.Reporter
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
	
	private def print(logger: F0[String] => Unit, posIn: Position, msg: String)
	{
		def log(s: => String) = logger(Message(s))
		// the implicits keep source compatibility with the changes in 2.8 : Position.{source,line,column} are no longer Options
		implicit def anyToOption[T <: AnyRef](t: T): Option[T] = Some(t)
		implicit def intToOption(t: Int): Option[Int] = Some(t)
		val pos =
			posIn match
			{
				case null | NoPosition => NoPosition
				case x: FakePos => x
				case x =>
					posIn.inUltimateSource(posIn.source.get)
			}
		pos match
		{
			case NoPosition => log(msg)
			case FakePos(fmsg) => log(fmsg+" "+msg)
			case _ =>
				val sourcePrefix = pos.source.map(_.file.path).getOrElse("")
				val lineNumberString = pos.line.map(line => ":" + line + ":").getOrElse(":") + " "
				log(sourcePrefix + lineNumberString + msg)
				if (!pos.line.isEmpty)
				{
					val lineContent = pos.lineContent.stripLineEnd
					log(lineContent) // source line with error/warning
					for(offset <- pos.offset; src <- pos.source)
					{
						val pointer = offset - src.lineToOffset(src.offsetToLine(offset))
						val pointerSpace = lineContent.take(pointer).map { case '\t' => '\t'; case x => ' ' }
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

	protected def info0(pos: Position, msg: String, severity: Severity, force: Boolean)
	{
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