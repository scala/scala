/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger
import scala.tools.nsc.SubComponent

class ScaladocInterface
{
	def run(args: Array[String], maximumErrors: Int, log: Logger) = (new Runner(args, maximumErrors, log)).run
}
private class Runner(args: Array[String], maximumErrors: Int, log: Logger)
{
	import scala.tools.nsc.{doc, CompilerCommand, Global}
	val reporter = new LoggerReporter(maximumErrors, log)
	val docSettings: doc.Settings = new doc.Settings(reporter.error)
	val command = new CompilerCommand(args.toList, docSettings, error, false)

	import forScope._
	def run()
	{
		if(!reporter.hasErrors)
		{
			import doc._ // 2.8 has doc.Processor
			val processor = new Processor(reporter, docSettings)
			processor.document(command.files)
		}
		reporter.printSummary()
		if(reporter.hasErrors) throw new InterfaceCompileFailed(args, "Scaladoc generation failed")
	}

	object forScope
	{
		class Processor(reporter: LoggerReporter, docSettings: doc.Settings) // 2.7 compatibility
		{
			object compiler extends Global(command.settings, reporter)
			{
				override def onlyPresentation = true
				class DefaultDocDriver  // 2.8 compatibility
				{
					assert(false)
					def process(units: Iterator[CompilationUnit]) = error("for 2.8 compatibility only")
				}
			}
			def document(ignore: Seq[String])
			{
				import compiler._
				val run = new Run
				run compile command.files

				val generator =
				{
					import doc._
					new DefaultDocDriver
					{
						lazy val global: compiler.type = compiler
						lazy val settings = docSettings
					}
				}
				generator.process(run.units)
			}
		}
	}
}