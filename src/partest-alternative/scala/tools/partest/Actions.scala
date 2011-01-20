/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest

import util._
import nsc.io._
import scala.sys.process._

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
        trace("runExec: " + execEnv.mkString("ENV(", "\n", "\n)"))
        execCwd foreach (x => trace("CWD(" + x + ")"))
      }

      trace("runExec: " + cmd)
      isDryRun || execAndLog(cmd)
    }

    /** Exec a process to run a command.  Assumes 0 exit value is success.
     *  Of necessity, also treats no available exit value as success.
     */
    protected def execAndLog(cmd: String) = (cmd #> logFile.jfile !) == 0
  }

  trait ScriptableTest {
    self: TestEntity =>

    /** Translates a line from a .cmds file into a teststep.
     */
    def customTestStep(line: String): TestStep = {
      trace("customTestStep: " + line)
      val (cmd, rest) = line span (x => !Character.isWhitespace(x))
      def qualify(name: String) = sourcesDir / name path
      val args = toArgs(rest) map qualify
      def fail: TestStep = (_: TestEntity) => error("Parse error: did not understand '%s'" format line)

      val f: TestEntity => Boolean = cmd match {
        case "scalac"   => _ scalac args
        case "javac"    => _ javac args
        case "scala"    => _ runScala args
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
      trace("compile: " + sourceFiles)

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
    def checkFileRequired =
      returning(checkFile.isFile)(res => if (!res) warnAndLog("A checkFile at '%s' is mandatory.\n" format checkFile.path))

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

    /** diffFiles requires actual Files as arguments but the output we want
     *  is the post-processed versions of log/check, so we resort to tempfiles.
     */
    lazy val diffOutput = {
      if (!checkFile.exists) "" else {
        val input   = diffCleanup(checkFile)
        val output  = diffCleanup(logFile)
        def asFile(s: String) = returning(File.makeTemp("partest-diff"))(_ writeAll s)

        if (input == output) ""
        else diffFiles(asFile(input), asFile(output))
      }
    }
    private def checkTraceName  = tracePath(checkFile)
    private def logTraceName    = tracePath(logFile)
    private def isDiffConfirmed = checkFile.exists && (diffOutput == "")

    private def sendTraceMsg() {
      def result =
        if (isDryRun) ""
        else if (isDiffConfirmed) " [passed]"
        else if (checkFile.exists) " [failed]"
        else " [unchecked]"

      trace("diff %s %s%s".format(checkTraceName, logTraceName, result))
    }

    /** If optional is true, a missing check file is considered
     *  a successful diff.  Necessary since many categories use
     *  checkfiles in an ad hoc manner.
     */
    def runDiff() = {
      sendTraceMsg()

      def updateCheck = (
        isUpdateCheck && {
          val formatStr = "** diff %s %s: " + (
            if (checkFile.exists) "failed, updating '%s' and marking as passed."
            else if (diffOutput == "") "not creating checkFile at '%s' as there is no output."
            else "was unchecked, creating '%s' for future tests."
          ) + "\n"

          normal(formatStr.format(checkTraceName, logTraceName, checkFile.path))
          if (diffOutput != "") normal(diffOutput)

          checkFile.writeAll(diffCleanup(logFile), "\n")
          true
        }
      )

      isDryRun || isDiffConfirmed || (updateCheck || !checkFile.exists)
    }
  }
}
