/* NSC -- new Scala compiler
 * Copyright 2006 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

import java.io.File


/** An object that runs Scala code.  It has three possible
  * sources for the code to run: pre-compiled code, a script file,
  * or interactive entry.
  */
object MainGenericRunner {
  def main(args: Array[String]): Unit = {
    def error(str: String) = Console.println(str)
    val command = new GenericRunnerCommand(args.toList, error)
    if (!command.ok) {
      Console.println(command.usageMessage)
      return ()
    }

    val settings = command.settings

    if (settings.help.value) {
      Console.println(command.usageMessage)
      return ()
    }

    if (settings.version.value) {
      val version =
        System.getProperty("scala.tool.version", "unknown version")
      Console.println("scala version " + version)
      Console.println("(c) 2002-2006 LAMP/EPFL")
      return ()
    }

    command.thingToRun match {
      case None =>
        (new InterpreterLoop).main(settings)

      case Some(thingToRun) =>
        val isObjectName =
          settings.howtorun.value match {
            case "guess" =>
              val f = new File(thingToRun)
              !f.exists || f.isDirectory
            case "object" => true
            case "script" => false
          }

        if (isObjectName) {
          def paths(str: String) = str.split(File.pathSeparator).toList

          val classpath =
            paths(settings.bootclasspath.value) :::
            paths(settings.classpath.value)

          ObjectRunner.run(classpath, thingToRun, command.arguments)
        } else {
          MainScript.runScript(settings, thingToRun, command.arguments)
        }
    }
  }
}
