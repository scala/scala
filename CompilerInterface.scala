/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{AnalysisCallback,AnalysisCallbackContainer,Logger}

class CompilerInterface
{
	def run(args: Array[String], callback: AnalysisCallback, maximumErrors: Int, log: Logger)
	{
		import scala.tools.nsc.{CompilerCommand, FatalError, Global, Settings, reporters, util}
		import util.FakePos
		val reporter = new LoggerReporter(maximumErrors, log)
		val settings = new Settings(reporter.error)
		val command = new CompilerCommand(args.toList, settings, error, false)
		
		object compiler extends Global(command.settings, reporter) with AnalysisCallbackContainer
		{
			def analysisCallback = callback
		}
		if(!reporter.hasErrors)
		{
			val run = new compiler.Run
			run compile command.files
			reporter.printSummary()
		}
		!reporter.hasErrors
	}
}