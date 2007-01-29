/* NSC -- new Scala compiler
 * Copyright 2006-2007 LAMP/EPFL
 * @author  Lex Spoon
 */

// $Id$

package scala.tools.nsc

import java.io.File
import java.lang.{ClassNotFoundException, NoSuchMethodException}
import java.lang.reflect.InvocationTargetException

/** An object that runs Scala code.  It has three possible
  * sources for the code to run: pre-compiled code, a script file,
  * or interactive entry.
  */
object MainGenericRunner {
  /** Append jars found in ${scala.home}/lib to
   *  a specified classpath.  Also append "." if the
   *  input classpath is empty; otherwise do not.
   *
   *  @param  classpath
   *  @return ...
   */
  private def addClasspathExtras(classpath: String): String = {
    val scalaHome = Properties.scalaHome

    val extraClassPath =
      if (scalaHome eq null)
        ""
      else {
        val libdir = new File(new File(scalaHome), "lib")
        if(!libdir.exists || libdir.isFile)
          return classpath

        val filesInLib = libdir.listFiles
        val jarsInLib =
          filesInLib.filter(f =>
            f.isFile && f.getName.endsWith(".jar"))

        jarsInLib.mkString("", File.pathSeparator, "")
      }

    if(classpath == "")
      extraClassPath + File.pathSeparator + "."
    else
      classpath + File.pathSeparator + extraClassPath
  }

  def main(args: Array[String]): Unit = {
    def error(str: String) = Console.println(str)
    val command = new GenericRunnerCommand(args.toList, error)
    if (!command.ok) {
      Console.println(command.usageMessage)
      return ()
    }

    val settings = command.settings

    settings.classpath.value =
      addClasspathExtras(settings.classpath.value)

    settings.defines.applyToCurrentJVM

    if (settings.help.value || !command.ok) {
      Console.println(command.usageMessage)
      return
    }

    if (settings.version.value) {
      Console.println(
        "Scala code runner " +
        Properties.versionString + " -- " +
        Properties.copyrightString)
      return
    }

    def paths(str: String) = str.split(File.pathSeparator).toList
    def listJars(dirs: String): List[String] =
      for (
        val libdir <- (paths(dirs) map { s => new File(s) }); libdir.exists; !libdir.isFile;
        val jar <- libdir.listFiles; jar.isFile; jar.getName.endsWith(".jar")
      ) yield jar.toString
    val classpath: List[String] =
      paths(settings.bootclasspath.value) :::
      paths(settings.classpath.value) :::
      listJars(settings.extdirs.value)

    command.thingToRun match {
      case None =>
        (new InterpreterLoop).main(settings)

      case Some(thingToRun) =>
        val isObjectName =
          settings.howtorun.value match {
            case "object" => true
            case "script" => false
            case "guess" =>
              ObjectRunner.classExists(classpath, thingToRun)
          }

        if (isObjectName) {

          try {
            ObjectRunner.run(classpath, thingToRun, command.arguments)
          } catch {
            case e: ClassNotFoundException =>
              Console.println(e)
            case e: NoSuchMethodException =>
              Console.println(e)
            case e: InvocationTargetException =>
              e.getCause.printStackTrace
          }
        } else {
          ScriptRunner.runScript(settings, thingToRun, command.arguments)
        }
    }
  }
}
