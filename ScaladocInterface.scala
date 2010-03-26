/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger
import Log.debug

class ScaladocInterface
{
	def run(args: Array[String], maximumErrors: Int, log: Logger) = (new Runner(args, maximumErrors, log)).run
}
private class Runner(args: Array[String], maximumErrors: Int, log: Logger)
{
	import scala.tools.nsc.{doc, Global}
	val reporter = new LoggerReporter(maximumErrors, log)
	val docSettings: doc.Settings = new doc.Settings(reporter.error)
	val command = Command(args.toList, docSettings)

	import forScope._
	def run()
	{
		debug(log, "Calling Scaladoc with arguments:\n\t" + args.mkString("\n\t"))
		if(!reporter.hasErrors)
		{
			import doc._ // 2.8 trunk and Beta1-RC4 have doc.DocFactory.  For other Scala versions, the next line creates forScope.DocFactory
			val processor = new DocFactory(reporter, docSettings)
			processor.document(command.files)
		}
		reporter.printSummary()
		if(reporter.hasErrors) throw new InterfaceCompileFailed(args, "Scaladoc generation failed")
	}

	object forScope
	{
		class DocFactory(reporter: LoggerReporter, docSettings: doc.Settings) // 2.7 compatibility
		{
			object compiler extends Global(command.settings, reporter)
			{
				override def onlyPresentation = true
				class DefaultDocDriver  // 2.8 source compatibility
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