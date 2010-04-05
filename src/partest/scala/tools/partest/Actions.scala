/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest

import util._
import nsc.io._

trait Actions {
  partest: Universe =>

  class TestSequence(val actions: List[TestStep]) extends AbsTestSequence {
  }

  implicit def createSequence(xs: List[TestStep]) = new TestSequence(xs)

  trait ExecSupport {
    self: TestEntity =>

    def execEnv: Map[String, String] = {
      val map = assembleEnvironment()
      val cwd = execCwd.toList map ("CWD" -> _.path)

      map ++ cwd
    }
    def execCwd = if (commandFile.isFile) Some(sourcesDir) else None

    def runExec(args: List[String]): Boolean = {
      val cmd     = fromArgs(args)

      if (isVerbose) {
        trace(execEnv.mkString("ENV(", "\n", "\n)"))
        execCwd foreach (x => trace("CWD(" + x + ")"))
      }

      trace(cmd)
      isDryRun || execAndLog(cmd)
    }

    /** Exec a process to run a command.  Assumes 0 exit value is success.
     *  Of necessity, also treats no available exit value as success.
     */
    protected def execAndLog(cmd: String): Boolean = {
      var proc: Process = null

      val result = interruptMeIn(testTimeout) {
        loggingResult {
          proc = Process.exec(toArgs(cmd), execEnv, execCwd.orNull, true)
          proc.slurp()
        }
        proc.waitFor() == 0
      }
      result getOrElse {
        warning("Process never terminated: '%s'" format cmd)
        if (proc != null)
          proc.destroy()

        false
      }
    }
  }

  trait ScriptableTest {
    self: TestEntity =>

    // def customTestStep(line: String): TestStep

    /** Translates a line from a .cmds file into a teststep.
     */
    def customTestStep(line: String): TestStep = {
      val (cmd, rest) = line span (x => !Character.isWhitespace(x))
      val args = toArgs(rest)
      def fail: TestStep = (_: TestEntity) => error("Parse error: did not understand '%s'" format line)

      val f: TestEntity => Boolean = cmd match {
        case "scalac"   => _ scalac args
        case "javac"    => _ javac args
        case "scala"    => _ runScala args
        case "diff"     => if (args.size != 2) fail else _ => diffFiles(File(args(0)), File(args(1))) == ""
        case _          => fail
      }
      f
    }
  }

  trait CompilableTest extends CompileExecSupport {
    self: TestEntity =>

    def sourceFiles   = location.walk collect { case f: File if isJavaOrScala(f) => f } toList
    def allSources    = sourceFiles map (_.path)
    def scalaSources  = sourceFiles filter isScala map (_.path)
    def javaSources   = sourceFiles filter isJava map (_.path)

    /** If there are mixed java and scala files, the standard compilation
     *  sequence is:
     *
     *    scalac with all files
     *    javac with only java files
     *    scalac with only scala files
     *
     *  This should be expanded to encompass other strategies so we know how
     *  well they're working or not working - notably, it would be very useful
     *  to know exactly when and how two-pass compilation fails.
     */
    def compile() = {
      def compileJava()   = javac(javaSources)
      def compileScala()  = scalac(scalaSources)
      def compileAll()    = scalac(allSources)
      def compileMixed()  = compileAll() && compileJava() && compileScala()

      if (scalaSources.nonEmpty && javaSources.nonEmpty) compileMixed()
      else compileScala()
    }
  }

  trait DiffableTest {
    self: TestEntity =>

    def checkFile: File   = withExtension("check").toFile

    def normalizePaths(s: String) = {
      /** This accomodates slash/backslash issues by noticing when a given
       *  line was altered, which means it held a path, and only then converting any
       *  backslashes to slashes.  It's not foolproof but it's as close as we
       *  can get in one line.
       */
      val s2 = s.replaceAll("""(?m)\Q%s\E""" format (sourcesDir + File.separator), "")
      if (s != s2) s2.replaceAll("""\\""", "/") else s2
    }

    /** The default cleanup normalizes paths relative to sourcesDir.
     */
    def diffCleanup(f: File) = safeLines(f) map normalizePaths mkString "\n"

    /** If optional is true, a missing check file is considered
     *  a successful diff.  Necessary since many categories use
     *  checkfiles in an ad hoc manner.
     */
    def runDiff(check: File, log: File) = {
      def arg1    = tracePath(check)
      def arg2    = tracePath(log)
      def noCheck = !check.exists && returning(true)(_ =>  trace("diff %s %s [unchecked]".format(arg1, arg2)))

      noCheck || {
        def result  = safeSlurp(check).trim == diffCleanup(log).trim
        def msg     = if (result) "passed" else "failed"

        if (isDryRun) {
          trace("diff %s %s".format(arg1, arg2))
          true
        }
        else {
          trace("diff %s %s [%s]".format(arg1, arg2, msg))
          result
        }
      }
    }

    private def cleanedLog    = returning(File makeTemp "partest-diff")(_ writeAll diffCleanup(logFile))
    def diffOutput(): String  = checkFile ifFile (f => diffFiles(f, cleanedLog)) getOrElse ""
  }
}
