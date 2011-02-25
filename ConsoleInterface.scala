/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import xsbti.Logger
import scala.tools.nsc.{GenericRunnerCommand, Interpreter, InterpreterLoop, ObjectRunner, Settings}
import scala.tools.nsc.interpreter.InteractiveReader
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.util.ClassPath

class ConsoleInterface
{
	def run(args: Array[String], bootClasspathString: String, classpathString: String, initialCommands: String, loader: ClassLoader, bindNames: Array[String], bindValues: Array[Any], log: Logger)
	{
		val options = args.toList
		lazy val interpreterSettings = xsbt.MakeSettings(options, log)
		val compilerSettings = xsbt.MakeSettings(options, log)
		
		if(!bootClasspathString.isEmpty)
			compilerSettings.bootclasspath.value = bootClasspathString
		compilerSettings.classpath.value = classpathString
		log.info(Message("Starting scala interpreter..."))
		log.debug(Message("  Boot classpath: " + compilerSettings.bootclasspath.value))
		log.debug(Message("  Classpath: " + compilerSettings.classpath.value))
		log.info(Message(""))
		val loop = new InterpreterLoop {
		
			override def createInterpreter() = {
			
				if(loader ne null)
				{
					in = InteractiveReader.createDefault()
					interpreter = new Interpreter(settings)
					{
						override protected def parentClassLoader = if(loader eq null) super.parentClassLoader else loader
						override protected def newCompiler(settings: Settings, reporter: Reporter) = super.newCompiler(compilerSettings, reporter)
					}
					interpreter.setContextClassLoader()
				}
				else
					super.createInterpreter()
					
				for( (id, value) <- bindNames zip bindValues)
					interpreter.beQuietDuring(interpreter.bind(id, value.asInstanceOf[AnyRef].getClass.getName, value))
			
				if(!initialCommands.isEmpty)
					interpreter.interpret(initialCommands)
			}
		}
		loop.main(if(loader eq null) compilerSettings else interpreterSettings)
	}
}
object MakeSettings
{
	def apply(args: List[String], log: Logger) =
	{
		val command = new GenericRunnerCommand(args, message => log.error(Message(message)))
		if(command.ok)
			command.settings
		else
			throw new InterfaceCompileFailed(Array(), Array(), command.usageMsg)
	}
}
