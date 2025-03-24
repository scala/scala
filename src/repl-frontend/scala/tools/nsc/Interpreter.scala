/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import scala.tools.nsc.interpreter.{IMain, Repl, ReplCore}
import scala.tools.nsc.interpreter.shell.{ILoop, ReplReporterImpl, ShellConfig}
import scala.tools.nsc.reporters.Reporter

import scala.language.implicitConversions

// Pretty gross contortion to satisfy the de facto interface expected by sbt.
// The idea is to have sbt stage a dummy interpreter, to extract the configuration
// it's trying to create, only to then actually create our interpreter when needed.

@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class Interpreter(ignored: Settings) {
  private[nsc] def config = (newCompiler(null, null), parentClassLoader)

  @deprecated("Only used for passing in the classloader.", "2.13.0-M2")
  protected def parentClassLoader: ClassLoader = null

  // ignore the method name -- only used to find out what old sbt versions want our compiler settings to be
  @deprecated("Only used for passing in settings.", "2.13.0-M2")
  protected def newCompiler(settings: Settings, reporter: Reporter) = settings
}

@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class InterpreterLoop {
  @deprecated("Ignored.", "2.9.0")
  var in: Any = null

  @deprecated("Ignored.", "2.13.0-M2")
  def settings: Settings = null

  private var interpreterSettings: Settings = _
  private var compilerSettings: Settings = _
  private var parentClassLoader: Option[ClassLoader] = _

  @deprecated("Mainly used for passing in settings.", "2.13.0-M2")
  def interpreter_= (interpreter: Interpreter) = {
    val config = interpreter.config
    compilerSettings = config._1 match { case null => interpreterSettings case cs => cs }
    parentClassLoader = Option(config._2)
  }

  @volatile private var intp: Repl = _

  def interpreter: ReplCore = {
    if (intp eq null) {
      intp = new IMain(interpreterSettings, parentClassLoader, compilerSettings, new ReplReporterImpl(interpreterSettings))
    }
    intp
  }

  @deprecated("Only used for passing in settings.", "2.13.0-M2")
  def createInterpreter(): Unit = {}

  def closeInterpreter(): Unit =
    if (intp ne null) {
      intp.close()
      intp = null
    }

  def main(interpreterSettings: Settings): Unit = {
    this.interpreterSettings = interpreterSettings

    // this call goes to the overridden method in sbt,
    // which may customize a subclass of Interpreter with some settings
    // if it does, it'll first call the setter for interpreter, and then the getter (to call setContextClassLoader)
    // in any case, it'll bind some values and interpret a preamble
    createInterpreter()

    val shell = new ILoop(ShellConfig(interpreterSettings))
    shell.intp = interpreter.asInstanceOf[Repl] // we've restricted the type of `interpreter` above to denote the subset used by sbt
    shell.run(interpreterSettings)
  }

  // sbt uses InterpreterLoop as follows -- the private method is an easy way to ensure we don't break it
  // TODO: turns this into a test
  // From https://github.com/sbt/sbt-zero-thirteen/blob/0.13/compile/interface/src/main/scala/xsbt/ConsoleInterface.scala
  // See also:
  //   - https://github.com/sbt/zinc/blob/1.0/internal/compiler-bridge/src/main/scala/xsbt/InteractiveConsoleInterface.scala
  //   - https://github.com/sbt/zinc/blob/1.0/internal/compiler-bridge/src/main/scala/xsbt/ConsoleInterface.scala
  @deprecated("Only here to ensure we don't break the sbt interface.", "2.13.0-M2")
  @annotation.unused
  private def __SbtConsoleInterface(compilerSettings: Settings, interpreterSettings: Settings, bootClasspathString: String, classpathString: String, initialCommands: String, cleanupCommands: String, loader: ClassLoader, bindNames: Array[String], bindValues: Array[Any]): Unit = {
    import scala.tools.nsc.interpreter.InteractiveReader
    import scala.tools.nsc.reporters.Reporter

    //  compilerSettings.bootclasspath.value = bootClasspathString
    //  compilerSettings.classpath.value = classpathString

    val loop = new InterpreterLoop {
      override def createInterpreter() = {
        if (loader eq null) super.createInterpreter()
        else {
          in = InteractiveReader.createDefault()
          interpreter = new Interpreter(settings) {
            override protected def parentClassLoader =
              if (loader eq null) super.parentClassLoader else loader

            override protected def newCompiler(settings: Settings, reporter: Reporter) =
              super.newCompiler(compilerSettings, reporter)
          }
        }

        // for 2.8 compatibility
        @annotation.unused
        final class Compat {
          def bindValue(id: String, value: Any) =
            interpreter.bind(id, value.asInstanceOf[AnyRef].getClass.getName, value)
        }
        @annotation.unused
        implicit def compat(a: AnyRef): Compat = new Compat

        interpreter.beQuietDuring(interpreter.bindValue(??? : String, ??? : Any))

        interpreter.interpret(??? : String)
      }

      override def closeInterpreter(): Unit = {
        interpreter.interpret(??? : String)
        super.closeInterpreter()
      }
    }
    loop.main(if (loader eq null) compilerSettings else interpreterSettings)
  }
}
