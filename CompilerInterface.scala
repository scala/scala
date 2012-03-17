/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{AnalysisCallback,Logger,Problem,Reporter,Severity}
import scala.tools.nsc.{Phase, SubComponent}
import Log.debug

class CompilerInterface
{
	def run(args: Array[String], callback: AnalysisCallback, log: Logger, delegate: Reporter)
	{
			import scala.tools.nsc.{Global, Settings}

		debug(log, "Interfacing (CompilerInterface) with Scala compiler " + scala.tools.nsc.Properties.versionString)

		val settings = new Settings(Log.settingsError(log))
		val command = Command(args.toList, settings)
		val dreporter = DelegatingReporter(settings, delegate)
		def noErrors = !dreporter.hasErrors && command.ok

		object compiler extends Global(command.settings, dreporter)
		{
			object dummy // temporary fix for #4426
			object sbtAnalyzer extends
			{
				val global: compiler.type = compiler
				val phaseName = Analyzer.name
				val runsAfter = List("jvm")
				override val runsBefore = List("terminal")
				val runsRightAfter = None
			}
			with SubComponent
			{
				val analyzer = new Analyzer(global, callback)
				def newPhase(prev: Phase) = analyzer.newPhase(prev)
				def name = phaseName
			}
			object apiExtractor extends
			{
				val global: compiler.type = compiler
				val phaseName = API.name
				val runsAfter = List("typer")
				override val runsBefore = List("erasure")
				val runsRightAfter = Some("typer")
			}
			with SubComponent
			{
				val api = new API(global, callback)
				def newPhase(prev: Phase) = api.newPhase(prev)
				def name = phaseName
			}
			
			override lazy val phaseDescriptors =
			{
				phasesSet += sbtAnalyzer
				phasesSet += apiExtractor
				superComputePhaseDescriptors
			}
			// Required because computePhaseDescriptors is private in 2.8 (changed to protected sometime later).
			private def superComputePhaseDescriptors() =
			{
				val meth = classOf[Global].getDeclaredMethod("computePhaseDescriptors")
				meth.setAccessible(true)
				meth.invoke(this).asInstanceOf[List[SubComponent]]
			}
			def logUnreportedWarnings(seq: List[(Position,String)]): Unit = // Scala 2.10.x and later
			{
				for( (pos, msg) <- seq) yield
					callback.problem(dreporter.convert(pos), msg, Severity.Warn, false)
			}
			def logUnreportedWarnings(count: Boolean): Unit = () // for source compatibility with Scala 2.8.x
			def logUnreportedWarnings(count: Int): Unit = () // for source compatibility with Scala 2.9.x
		}
		def processUnreportedWarnings(run: compiler.Run)
		{
				implicit def listToBoolean[T](l: List[T]): Boolean = error("source compatibility only, should never be called")
				implicit def listToInt[T](l: List[T]): Int = error("source compatibility only, should never be called")
			compiler.logUnreportedWarnings(run.deprecationWarnings)
			compiler.logUnreportedWarnings(run.uncheckedWarnings)
		}
		if(command.shouldStopWithInfo)
		{
			dreporter.info(null, command.getInfoMessage(compiler), true)
			throw new InterfaceCompileFailed(args, Array(), "Compiler option supplied that disabled actual compilation.")
		}
		if(noErrors)
		{
			val run = new compiler.Run
			debug(log, args.mkString("Calling Scala compiler with arguments  (CompilerInterface):\n\t", "\n\t", ""))
			run compile command.files
			processUnreportedWarnings(run)
			dreporter.problems foreach { p => callback.problem(p.position, p.message, p.severity, true) }
		}
		dreporter.printSummary()
		if(!noErrors)
		{
			debug(log, "Compilation failed (CompilerInterface)")
			throw new InterfaceCompileFailed(args, dreporter.problems, "Compilation failed")
		}
	}
}
class InterfaceCompileFailed(val arguments: Array[String], val problems: Array[Problem], override val toString: String) extends xsbti.CompileFailed