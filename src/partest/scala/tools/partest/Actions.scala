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

      val result = interruptMeIn(cmd, testTimeout) {
        loggingResult {
          proc = Process.exec(toArgs(cmd), execEnv, execCwd.orNull, true)
          proc.slurp()
        }
        proc != null && (proc.waitFor() == 0)
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
    def isCheckPresent    = checkFile.isFile || {
      warnAndLog("A checkFile at '%s' is mandatory.\n" format checkFile.path)
      false
    }
    lazy val sourceFileNames = sourceFiles map (_.name)

    /** Given the difficulty of verifying that any selective approach works
     *  everywhere, the algorithm now is to look for the name of any known
     *  source file for this test, and if seen, remove all the non-whitespace
     *  preceding it.  (Paths with whitespace don't work anyway.) This should
     *  wipe out all slashes, backslashes, C:\, cygwin/windows differences,
     *  and whatever else makes a simple diff not simple.
     *
     *  The log and check file are both transformed, which I don't think is
     *  correct -- only the log should be -- but doing it this way until I
     *  can clarify martin's comments in #3283.
     */
    def normalizePaths(s: String) =
      sourceFileNames.foldLeft(s)((res, name) => res.replaceAll("""\S+\Q%s\E""" format name, name))

    /** The default cleanup normalizes paths relative to sourcesDir,
     *  absorbs line terminator differences by going to lines and back,
     *  and trims leading or trailing whitespace.
     */
    def diffCleanup(f: File) = safeLines(f) map normalizePaths mkString "\n" trim

    /** If optional is true, a missing check file is considered
     *  a successful diff.  Necessary since many categories use
     *  checkfiles in an ad hoc manner.
     */
    def runDiff(check: File, log: File) = {
      def arg1    = tracePath(check)
      def arg2    = tracePath(log)
      def noCheck = !check.exists && returning(true)(_ =>  trace("diff %s %s [unchecked]".format(arg1, arg2)))
      def input   = diffCleanup(check)
      def output  = diffCleanup(log)
      def matches = input == output

      def traceMsg =
        if (isDryRun) "diff %s %s".format(arg1, arg2)
        else "diff %s %s [%s]".format(arg1, arg2, (if (matches) "passed" else "failed"))

      def updateCheck = (
        isUpdateCheck && {
          if (check.exists) {
            normal("** diff %s %s failed:\n".format(arg1, arg2))
            normal(diffOutput())
          }
          val verb = if (check.exists) "updating" else "creating"
          normal("** %s %s and marking as passed.\n".format(verb, arg1))
          check writeAll output
          true
        }
      )

      if (noCheck) returning(true)(_ => updateCheck)
      else {
        trace(traceMsg)
        isDryRun || matches || updateCheck
      }
    }

    private def cleanedLog    = returning(File makeTemp "partest-diff")(_ writeAll diffCleanup(logFile))
    def diffOutput(): String  = checkFile ifFile (f => diffFiles(f, cleanedLog)) getOrElse ""
  }
}
