/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools
package partest

import scala.tools.nsc.io._
import scala.tools.nsc.{ Global, Settings, CompilerCommand, FatalError }
import scala.tools.nsc.util.{ ClassPath }
import scala.tools.nsc.reporters.{ Reporter, ConsoleReporter }

trait PartestCompilation {
  self: Universe =>

  trait CompileExecSupport extends ExecSupport {
    self: TestEntity =>

    def javacpArg   = "-classpath " + testClasspath
    def scalacpArg  = "-usejavacp"

    /** Not used, requires tools.jar.
     */
    // def javacInternal(args: List[String]) = {
    //   import com.sun.tools.javac.Main
    //   Main.compile(args.toArray, logWriter)
    // }

    def javac(args: List[String]): Boolean = {
      val allArgString = fromArgs(javacpArg :: javacOpts :: args)

      // javac -d outdir -classpath <basepath> <files>
      val cmd = "%s -d %s %s".format(javacCmd, outDir, allArgString)
      def traceMsg =
        if (isVerbose) cmd
        else "%s -d %s %s".format(tracePath(Path(javacCmd)), tracePath(outDir), fromArgs(args))

      trace(traceMsg)

      isDryRun || execAndLog(cmd)
    }

    def scalac(args: List[String]): Boolean = {
      val allArgs = assembleScalacArgs(args)
      val (global, files) = newGlobal(allArgs)
      val foundFiles = execCwd match {
        case Some(cwd)  => files map (x => File(cwd / x))
        case _          => files map (x => File(x))
      }
      def nonFileArgs = if (isVerbose) global.settings.recreateArgs else assembleScalacArgs(Nil)
      def traceArgs   = fromArgs(nonFileArgs ++ (foundFiles map tracePath))
      def traceMsg    =
        if (isVerbose) "%s %s".format(build.scalaBin / "scalac", traceArgs)
        else "scalac " + traceArgs

      trace(traceMsg)
      isDryRun || global.partestCompile(foundFiles map (_.path), true)
    }

    /** Actually running the test, post compilation.
     *  Normally args will be List("Test", "jvm"), main class and arg to it.
     */
    def runScala(args: List[String]): Boolean = {
      val scalaRunnerClass = "scala.tools.nsc.MainGenericRunner"

      // java $JAVA_OPTS <javaopts> -classpath <cp>
      val javaCmdAndOptions = javaCmd +: assembleJavaArgs(List(javacpArg))
      // MainGenericRunner -usejavacp <scalacopts> Test jvm
      val scalaCmdAndOptions = List(scalaRunnerClass, scalacpArg) ++ assembleScalacArgs(args)
      // Assembled
      val cmd = fromArgs(javaCmdAndOptions ++ createPropertyString() ++ scalaCmdAndOptions)

      def traceMsg = if (isVerbose) cmd else fromArgs(javaCmd :: args)
      trace(traceMsg)

      isDryRun || execAndLog(cmd)
    }

    def newReporter(settings: Settings) = new ConsoleReporter(settings, Console.in, logWriter)

    class PartestGlobal(settings: Settings, val creporter: ConsoleReporter) extends Global(settings, creporter) {
      def partestCompile(files: List[String], printSummary: Boolean): Boolean = {
        try   { new Run compile files }
        catch { case FatalError(msg) => creporter.error(null, "fatal error: " + msg) }

        if (printSummary)
          creporter.printSummary

        creporter.flush()
        !creporter.hasErrors
      }
    }

    def newGlobal(args: List[String]): (PartestGlobal, List[String]) = {
      val settings  = category createSettings self
      val command   = new CompilerCommand(args, settings)
      val reporter  = newReporter(settings)

      if (!command.ok)
        debug("Error parsing arguments: '%s'".format(args mkString ", "))

      (new PartestGlobal(command.settings, reporter), command.files)
    }
  }
}