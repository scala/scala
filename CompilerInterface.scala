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
			def debug(msg: => String) = log.debug(Message(msg))
			import scala.tools.nsc.{CompilerCommand, Global, Settings}

		debug("Interfacing (CompilerInterface) with Scala compiler " + scala.tools.nsc.Properties.versionString)

		val reporter = new LoggerReporter(maximumErrors, log)
		val settings = new Settings(reporter.error)
		val command = new CompilerCommand(args.toList, settings, error, false)

		val phasesSet = new scala.collection.mutable.HashSet[Any] // 2.7 compatibility
		object compiler extends Global(command.settings, reporter)
		{
			object sbtAnalyzer extends
			{
				val global: compiler.type = compiler
				val phaseName = Analyzer.name
				val runsAfter = List("jvm")
				override val runsBefore = List("terminal")
				val runsRightAfter = None
			}
			with SubComponent with Compat27
			{
				val analyzer = new Analyzer(global, callback)
				def newPhase(prev: Phase) = analyzer.newPhase(prev)
				def name = phaseName
			}
			override def computePhaseDescriptors = // done this way for compatibility between 2.7 and 2.8
			{
				phasesSet += sbtAnalyzer
				val superd = super.computePhaseDescriptors
				if(superd.contains(sbtAnalyzer)) superd else ( superd ++ Seq(sbtAnalyzer) ).toList
			}
			trait Compat27 { val runsBefore: List[String] = Nil }
		}
		if(!reporter.hasErrors)
		{
			val run = new compiler.Run
			debug(args.mkString("Calling compiler with arguments  (CompilerInterface):\n\t", "\n\t", ""))
			run compile command.files
		}
		reporter.printSummary()
		if(reporter.hasErrors)
		{
			debug("Compilation failed (CompilerInterface)")
			throw new InterfaceCompileFailed(args, "Analyzed compilation failed")
		}
	}
}
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
class InterfaceCompileFailed(val arguments: Array[String], override val toString: String) extends xsbti.CompileFailed