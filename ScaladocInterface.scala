/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger
import scala.tools.nsc.SubComponent

class ScaladocInterface
{
	def run(args: Array[String], maximumErrors: Int, log: Logger)
	{
		import scala.tools.nsc.{doc, CompilerCommand, Global}
		val reporter = new LoggerReporter(maximumErrors, log)
		val docSettings: doc.Settings = new doc.Settings(reporter.error)
		val command = new CompilerCommand(args.toList, docSettings, error, false)
		trait Compat27 { def computeInternalPhases(): Unit = () }
		val phasesSet = scala.collection.mutable.Set[scala.tools.nsc.SubComponent]() // for 2.7 source compatibility
		object compiler extends Global(command.settings, reporter) with Compat27
		{
			override def onlyPresentation = true
			override def computeInternalPhases() {
				phasesSet += syntaxAnalyzer
				phasesSet += analyzer.namerFactory
				phasesSet += analyzer.typerFactory
			}
		}
		if(!reporter.hasErrors)
		{
			val run = new compiler.Run
			run compile command.files
			val generator = new doc.DefaultDocDriver
			{
				lazy val global: compiler.type = compiler
				lazy val settings = docSettings
			}
			generator.process(run.units)
		}
		reporter.printSummary()
		if(reporter.hasErrors) throw new InterfaceCompileFailed(args, "Scaladoc generation failed")
	}
}