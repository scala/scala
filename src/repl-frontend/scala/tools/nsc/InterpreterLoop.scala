package scala.tools.nsc

@deprecated("Use a class in the scala.tools.nsc.interpreter package.", "2.9.0")
class InterpreterLoop extends interpreter.shell.ILoop {
  @deprecated("Unused.", "2.9.0")
  var in: Any = null


  // sbt uses InterpreterLoop as follows -- the private method is an easy way to ensure we don't break it
  // TODO: turns this into a test
  // From https://github.com/sbt/sbt-zero-thirteen/blob/0.13/compile/interface/src/main/scala/xsbt/ConsoleInterface.scala
  // See also:
  //   - https://github.com/sbt/zinc/blob/1.0/internal/compiler-bridge/src/main/scala/xsbt/InteractiveConsoleInterface.scala
  //   - https://github.com/sbt/zinc/blob/1.0/internal/compiler-bridge/src/main/scala/xsbt/ConsoleInterface.scala
  @deprecated("Only here to ensure we don't break the sbt interface.", "2.13.0-M2")
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
          interpreter.setContextClassLoader()
        }

        // for 2.8 compatibility
        final class Compat {
          def bindValue(id: String, value: Any) =
            interpreter.bind(id, value.asInstanceOf[AnyRef].getClass.getName, value)
        }
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
