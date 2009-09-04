/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{AnalysisCallback,Logger}
import scala.tools.nsc.{Phase, SubComponent}

class CompilerInterface
{
	def run(args: Array[String], callback: AnalysisCallback, maximumErrors: Int, log: Logger)
	{
		import scala.tools.nsc.{CompilerCommand, FatalError, Global, Settings, reporters, util}
		import util.FakePos
		val reporter = new LoggerReporter(maximumErrors, log)
		val settings = new Settings(reporter.error)
		val command = new CompilerCommand(args.toList, settings, error, false)

		object compiler extends Global(command.settings, reporter)
		{
			object sbtAnalyzer extends
			{
				val global: compiler.type = compiler
				val phaseName = Analyzer.name
				val runsAfter = List("jvm")
				val runsRightAfter = None
			}
			with SubComponent
			{
				val analyzer = new Analyzer(global, callback)
				def newPhase(prev: Phase) = analyzer.newPhase(prev)
				def name = phaseName
			}
			override protected def builtInPhaseDescriptors() = (super.builtInPhaseDescriptors ++ Seq(sbtAnalyzer))
			/*override protected def computeInternalPhases()
			{
				super.computeInternalPhases()
				phasesSet += sbtAnalyzer
			}*/
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