/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.{AnalysisCallback,Logger}
import scala.tools.nsc.{Phase, SubComponent}
import Log.debug

class CompilerInterface
{
	def run(args: Array[String], callback: AnalysisCallback, maximumErrors: Int, log: Logger)
	{
			import scala.tools.nsc.{Global, Settings}

		debug(log, "Interfacing (CompilerInterface) with Scala compiler " + scala.tools.nsc.Properties.versionString)

		val reporter = new LoggerReporter(maximumErrors, log)
		val settings = new Settings(reporter.error)
		
		val command = Command(args.toList, settings)

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
			object apiExtractor extends
			{
				val global: compiler.type = compiler
				val phaseName = API.name
				val runsAfter = List("typer")
				override val runsBefore = List("erasure")
				val runsRightAfter = Some("typer")
			}
			with SubComponent with Compat27
			{
				val api = new API(global, callback)
				def newPhase(prev: Phase) = api.newPhase(prev)
				def name = phaseName
			}
			
			override lazy val phaseDescriptors = // done this way for compatibility between 2.7 and 2.8
			{
				phasesSet += sbtAnalyzer
				phasesSet += apiExtractor
				val superd = superComputePhaseDescriptors
				if(superd.contains(sbtAnalyzer))
					superd
				else
				{
					val typerIndex = superd.indexOf(analyzer.typerFactory)
					assert(typerIndex >= 0)
					superd.take(typerIndex+1) ::: apiExtractor :: superd.drop(typerIndex+1) ::: List(sbtAnalyzer)
				}
			}
			private def superComputePhaseDescriptors() = // required because 2.8 makes computePhaseDescriptors private
			{
				val meth = classOf[Global].getDeclaredMethod("computePhaseDescriptors")
				meth.setAccessible(true)
				meth.invoke(this).asInstanceOf[List[SubComponent]]
			}
			trait Compat27 { val runsBefore: List[String] = Nil }
		}
		if(!reporter.hasErrors)
		{
			val run = new compiler.Run
			debug(log, args.mkString("Calling Scala compiler with arguments  (CompilerInterface):\n\t", "\n\t", ""))
			run compile command.files
		}
		reporter.printSummary()
		if(reporter.hasErrors)
		{
			debug(log, "Compilation failed (CompilerInterface)")
			throw new InterfaceCompileFailed(args, "Compilation failed")
		}
	}
}
class InterfaceCompileFailed(val arguments: Array[String], override val toString: String) extends xsbti.CompileFailed