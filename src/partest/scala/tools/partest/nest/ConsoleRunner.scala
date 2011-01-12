/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.{File, PrintStream, FileOutputStream, BufferedReader,
                InputStreamReader, StringWriter, PrintWriter}
import utils.Properties._
import RunnerUtils._
import scala.tools.nsc.Properties.{ versionMsg, setProp }
import scala.tools.nsc.util.CommandLineParser
import scala.tools.nsc.io
import io.{ Path, Process }

class ConsoleRunner extends DirectRunner {
  import PathSettings.{ srcDir, testRoot }

  case class TestSet(kind: String, filter: Path => Boolean, msg: String)

  val testSets = {
    val pathFilter: Path => Boolean = _ hasExtension "scala"

    List(
      TestSet("pos", pathFilter, "Testing compiler (on files whose compilation should succeed)"),
      TestSet("neg", pathFilter, "Testing compiler (on files whose compilation should fail)"),
      TestSet("run", pathFilter, "Testing interpreter and backend"),
      TestSet("jvm", pathFilter, "Testing JVM backend"),
      TestSet("res", x => x.isFile && (x hasExtension "res"), "Testing resident compiler"),
      TestSet("buildmanager", _.isDirectory, "Testing Build Manager"),
      TestSet("shootout", pathFilter, "Testing shootout tests"),
      TestSet("script", pathFilter, "Testing script tests"),
      TestSet("scalacheck", x => pathFilter(x) || x.isDirectory, "Testing ScalaCheck tests"),
      TestSet("scalap", _.isDirectory, "Run scalap decompiler tests")
    )
  }

  var fileManager: ConsoleFileManager = _

  private var testFiles: List[File] = List()
  private val errors = PartestDefaults.errorCount
  private val testSetKinds  = testSets map (_.kind)
  private val testSetArgs   = testSets map ("--" + _.kind)
  private val testSetArgMap = testSetArgs zip testSets toMap

  def denotesTestSet(arg: String) = testSetArgs contains arg
  def denotesTestFile(arg: String) = (arg endsWith ".scala") || (arg endsWith ".res")
  def denotesTestDir(arg: String) = Path(arg).isDirectory
  def denotesTestPath(arg: String) = denotesTestDir(arg) || denotesTestFile(arg)

  private def printVersion { NestUI outline (versionMsg + "\n") }

  private val unaryArgs = List(
    "--pack", "--all", "--verbose", "--show-diff", "--show-log",
    "--failed", "--update-check", "--version", "--ansi", "--debug"
  ) ::: testSetArgs

  private val binaryArgs = List(
    "--grep", "--srcpath", "--buildpath", "--classpath"
  )

  def main(argstr: String) {
    val parsed = CommandLineParser(argstr) withUnaryArgs unaryArgs withBinaryArgs binaryArgs
    val args = parsed.residualArgs

    /** Early return on no args, version, or invalid args */
    if (argstr == "") return NestUI.usage()
    if (parsed isSet "--version") return printVersion
    if (args exists (x => !denotesTestPath(x))) {
      val invalid = (args filterNot denotesTestPath).head
      NestUI.failure("Invalid argument '%s'\n" format invalid)
      return NestUI.usage()
    }

    parsed get "--srcpath" foreach (x => setProp("partest.srcdir", x))

    fileManager =
      if (parsed isSet "--buildpath") new ConsoleFileManager(parsed("--buildpath"))
      else if (parsed isSet "--classpath") new ConsoleFileManager(parsed("--classpath"), true)
      else if (parsed isSet "--pack") new ConsoleFileManager("build/pack")
      else new ConsoleFileManager  // auto detection, see ConsoleFileManager.findLatest

    def argNarrowsTests(x: String) = denotesTestSet(x) || denotesTestFile(x) || denotesTestDir(x)

    NestUI._verbose         = parsed isSet "--verbose"
    fileManager.showDiff    = true
    // parsed isSet "--show-diff"
    fileManager.updateCheck = parsed isSet "--update-check"
    fileManager.showLog     = parsed isSet "--show-log"
    fileManager.failed      = parsed isSet "--failed"

    if (parsed isSet "--ansi") NestUI initialize NestUI.MANY
    if (parsed isSet "--timeout") fileManager.timeout = parsed("--timeout")
    if (parsed isSet "--debug") setProp("partest.debug", "true")

    setProperties() // must be done after processing command line arguments such as --debug

    def addTestFile(file: File) = {
      if (!file.exists)
        NestUI.failure("Test file '%s' not found, skipping.\n" format file)
      else {
        NestUI.verbose("adding test file " + file)
        testFiles +:= file
      }
    }

    // If --grep is given we suck in every file it matches.
    parsed get "--grep" foreach { expr =>
      val allFiles = srcDir.deepList() filter (_ hasExtension "scala") map (_.toFile) toList
      val files = allFiles filter (_.slurp() contains expr)

      if (files.isEmpty) NestUI.failure("--grep string '%s' matched no files." format expr)
      else NestUI.verbose("--grep string '%s' matched %d file(s)".format(expr, files.size))

      files foreach (x => addTestFile(x.jfile))
    }
    args foreach (x => addTestFile(new File(x)))

    // If no file arguments were given, we assume --all
    val enabledTestSets: List[TestSet] = {
      val enabledArgs = testSetArgs filter parsed.isSet

      if (args.isEmpty && !(parsed isSet "--grep") && (enabledArgs.isEmpty || (parsed isSet "--all"))) testSets
      else enabledArgs map testSetArgMap
    }

    val dir =
      if (fileManager.testClasses.isDefined) fileManager.testClassesDir
      else fileManager.testBuildFile getOrElse {
        fileManager.latestCompFile.getParentFile.getParentFile.getCanonicalFile
      }

    val vmBin  = javaHome + File.separator + "bin"
    val vmName = "%s (build %s, %s)".format(javaVmName, javaVmVersion, javaVmInfo)
    val vmOpts = fileManager.JAVA_OPTS

    NestUI.verbose("enabled test sets: " + (enabledTestSets map (_.kind) mkString " "))

    List(
      "Scala compiler classes in: " + dir,
      "Scala version is:          " + versionMsg,
      "Scalac options are:        " + fileManager.SCALAC_OPTS,
      "Java binaries in:          " + vmBin,
      "Java runtime is:           " + vmName,
      "Java options are:          " + vmOpts,
      "Source directory is:       " + srcDir,
      ""
    ) foreach (x => NestUI outline (x + "\n"))

    NestUI.verbose("available processors: " + Runtime.getRuntime().availableProcessors())

    val start = System.currentTimeMillis
    val (successes, failures) = testCheckAll(enabledTestSets)
    val end = System.currentTimeMillis

    val total = successes + failures

    val elapsedSecs = (end - start)/1000
    val elapsedMins = elapsedSecs/60
    val elapsedHrs  = elapsedMins/60
    val dispMins = elapsedMins - elapsedHrs  * 60
    val dispSecs = elapsedSecs - elapsedMins * 60

    val dispElapsed = {
      def form(num: Long) = if (num < 10) "0"+num else ""+num
      form(elapsedHrs)+":"+form(dispMins)+":"+form(dispSecs)
    }

    println
    if (failures == 0)
      NestUI.success("All of "+total+" tests were successful (elapsed time: "+dispElapsed+")\n")
    else
      NestUI.failure(failures+" of "+total+" tests failed (elapsed time: "+dispElapsed+")\n")

    System exit ( if (failures == errors) 0 else 1 )
  }

  def runTests(testSet: TestSet): (Int, Int) = {
    val TestSet(kind, filter, msg) = testSet

    fileManager.getFiles(kind, filter) match {
      case Nil    => NestUI.verbose("test dir empty\n") ; (0, 0)
      case files  =>
        NestUI.verbose("test files: "+files)
        NestUI.outline("\n"+msg+"\n")
        resultsToStatistics(runTestsForFiles(files, kind))
    }
  }

  /**
   * @return (success count, failure count)
   */
  def testCheckAll(enabledSets: List[TestSet]): (Int, Int) = {
    def kindOf(f: File) = (srcDir relativize Path(f).normalize).segments.head

    val (valid, invalid) = testFiles partition (x => testSetKinds contains kindOf(x))
    invalid foreach (x => NestUI.failure("Invalid test file '%s', skipping.\n" format x))

    val runTestsFileLists =
      for ((kind, files) <- valid groupBy kindOf toList) yield {
        NestUI.outline("\nTesting individual files\n")
        resultsToStatistics(runTestsForFiles(files, kind))
      }

    if (enabledSets.nonEmpty)
      NestUI.verbose("Run sets: "+enabledSets)

    val results = runTestsFileLists ::: (enabledSets map runTests)

    (results map (_._1) sum, results map (_._2) sum)
  }
}
