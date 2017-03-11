package xsbtpamflet

import java.io.{ PrintWriter, StringWriter }

import xsbt.Message
import xsbti.Logger
import xsbtpamflet.ConsoleHelper._

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.{ GenericRunnerCommand, Settings }

class ConsoleInterface(args: Array[String], bootClasspathString: String,
  classpathString: String, initialCommands: String, cleanupCommands: String,
  loader: ClassLoader, bindNames: Array[String], bindValues: Array[AnyRef],
  log: Logger) extends xsbti.ConsoleInterface {
  lazy val interpreterSettings = MakeSettings.sync(args.toList, { message => log.error(Message(message)) })
  val compilerSettings = MakeSettings.sync(args, bootClasspathString, classpathString, { message => log.error(Message(message)) })
  if (!bootClasspathString.isEmpty)
    compilerSettings.bootclasspath.value = bootClasspathString
  compilerSettings.classpath.value = classpathString
  val outWriter: StringWriter = new StringWriter
  val poutWriter: PrintWriter = new PrintWriter(outWriter)

  // log.info(Message("Starting scala interpreter..."))
  // log.info(Message(""))

  // val loop = new InterpreterLoop(None, poutWriter) {
  //   override def createInterpreter() = {
  //     if (loader ne null) {
  //       in = InteractiveReader.createDefault()
  //       interpreter = new Interpreter(settings) {
  //         override protected def parentClassLoader = if (loader eq null) super.parentClassLoader else loader
  //         override protected def newCompiler(settings: Settings, reporter: Reporter) = super.newCompiler(compilerSettings, reporter)
  //       }
  //       interpreter.setContextClassLoader()
  //     } else
  //       super.createInterpreter()
  //
  //     def bind(values: Seq[(String, Any)]) {
  //       // for 2.8 compatibility
  //       final class Compat {
  //         def bindValue(id: String, value: Any) =
  //           interpreter.bind(id, value.asInstanceOf[AnyRef].getClass.getName, value)
  //       }
  //       implicit def compat(a: AnyRef): Compat = new Compat
  //       for ((id, value) <- values)
  //         interpreter.beQuietDuring(interpreter.bindValue(id, value))
  //     }
  //
  //     bind(bindNames zip bindValues)
  //
  //     if (!initialCommands.isEmpty)
  //       interpreter.interpret(initialCommands)
  //   }
  //   override def closeInterpreter() {
  //     if (!cleanupCommands.isEmpty)
  //       interpreter.interpret(cleanupCommands)
  //     super.closeInterpreter()
  //   }
  // }

  val interpreter: IMain = new IMain(compilerSettings, new PrintWriter(outWriter)) {
    def lastReq = prevRequestList.last
  }

  // val interpreter = new Interpreter(compilerSettings) {
  // TODO: Fix this
  // override protected def parentClassLoader = if (loader eq null) super.parentClassLoader else loader
  // override protected def newCompiler(settings: Settings, reporter: Reporter) = super.newCompiler(compilerSettings, reporter)
  //}
  def interpret(line: String, synthetic: Boolean): ConsoleResponse =
    {
      clearBuffer()
      val r = interpreter.interpret(line, synthetic)
      ConsoleResponse(r, outWriter.toString)
    }
  def clearBuffer(): Unit = {
    // errorWriter.getBuffer.setLength(0)
    outWriter.getBuffer.setLength(0)
  }

  def reset(): Unit = {
    clearBuffer()
    interpreter.reset()
  }
}

object MakeSettings {
  def apply(args: List[String], onError: String => Unit) =
    {
      val command = new GenericRunnerCommand(args, onError(_))
      if (command.ok) command.settings
      // TODO: Provide better exception
      else throw new Exception(command.usageMsg)
    }

  def sync(args: Array[String], bootClasspathString: String, classpathString: String, onError: String => Unit): Settings =
    {
      val compilerSettings = sync(args.toList, onError)
      if (!bootClasspathString.isEmpty)
        compilerSettings.bootclasspath.value = bootClasspathString
      compilerSettings.classpath.value = classpathString
      compilerSettings
    }

  def sync(options: List[String], onError: String => Unit) =
    {
      val settings = apply(options, onError)

      // -Yrepl-sync is only in 2.9.1+
      final class Compat {
        def Yreplsync = settings.BooleanSetting("-Yrepl-sync", "For compatibility only.")
      }
      implicit def compat(s: Settings): Compat = new Compat

      settings.Yreplsync.value = true
      settings
    }
}
