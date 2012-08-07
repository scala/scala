/* NEST (New Scala Test)
 * Copyright 2007-2012 LAMP/EPFL
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
import io.{ Path }
import scala.collection.{ mutable, immutable }

class ConsoleRunner extends DirectRunner {
  import PathSettings.{ srcDir, testRoot }

  case class TestSet(kind: String, filter: Path => Boolean, msg: String)
  private def stdFilter(p: Path) = p.isDirectory || (p hasExtension "scala")
  private def antFilter(p: Path) = p.isFile && (p endsWith "build.xml")

  val testSets = {
    val pathFilter: Path => Boolean = x => x.isDirectory || (x hasExtension "scala")

    List(
      TestSet("pos", stdFilter, "Testing compiler (on files whose compilation should succeed)"),
      TestSet("neg", stdFilter, "Testing compiler (on files whose compilation should fail)"),
      TestSet("run", stdFilter, "Testing interpreter and backend"),
      TestSet("jvm", stdFilter, "Testing JVM backend"),
      TestSet("res", x => x.isFile && (x hasExtension "res"), "Testing resident compiler"),
      TestSet("buildmanager", _.isDirectory, "Testing Build Manager"),
      TestSet("shootout", stdFilter, "Testing shootout tests"),
      TestSet("script", stdFilter, "Testing script tests"),
      TestSet("scalacheck", stdFilter, "Testing ScalaCheck tests"),
      TestSet("scalap", _.isDirectory, "Run scalap decompiler tests"),
      TestSet("specialized", stdFilter, "Testing specialized tests"),
      TestSet("instrumented", stdFilter, "Testing instrumented tests"),
      TestSet("presentation", _.isDirectory, "Testing presentation compiler tests."),
      TestSet("ant", antFilter, "Run Ant task tests.")
    )
  }

  var fileManager: ConsoleFileManager = _

  private var testFiles: List[File] = List()
  private val errors = PartestDefaults.errorCount
  private val testSetKinds  = testSets map (_.kind)
  private val testSetArgs   = testSets map ("--" + _.kind)
  private val testSetArgMap = testSetArgs zip testSets toMap

  def denotesTestSet(arg: String)  = testSetArgs contains arg

  private def printVersion() { NestUI outline (versionMsg + "\n") }

  private val unaryArgs = List(
    "--pack", "--all", "--verbose", "--show-diff", "--show-log",
    "--failed", "--update-check", "--version", "--ansi", "--debug", "--help"
  ) ::: testSetArgs

  private val binaryArgs = List(
    "--grep", "--srcpath", "--buildpath", "--classpath"
  )

  // true if a test path matches the --grep expression.
  private def pathMatchesExpr(path: Path, expr: String) = {
    def pred(p: Path) = file2String(p.toFile) contains expr
    def srcs = path.toDirectory.deepList() filter (_.hasExtension("scala", "java"))

    (path.isFile && pred(path)) ||
    (path.isDirectory && srcs.exists(pred)) ||
    (pred(path changeExtension "check"))
  }

  def main(argstr: String) {
    val parsed = CommandLineParser(argstr) withUnaryArgs unaryArgs withBinaryArgs binaryArgs
    val args   = onlyValidTestPaths(parsed.residualArgs)

    /** Early return on no args, version, or invalid args */
    if (argstr == "") return NestUI.usage()
    if (parsed isSet "--version") return printVersion
    if (parsed isSet "--help") return NestUI.usage()

    parsed get "--srcpath" foreach (x => setProp("partest.srcdir", x))

    fileManager =
      if (parsed isSet "--buildpath") new ConsoleFileManager(parsed("--buildpath"))
      else if (parsed isSet "--classpath") new ConsoleFileManager(parsed("--classpath"), true)
      else if (parsed isSet "--pack") new ConsoleFileManager("build/pack")
      else new ConsoleFileManager  // auto detection, see ConsoleFileManager.findLatest

    def argNarrowsTests(x: String) = denotesTestSet(x) || denotesTestPath(x)

    NestUI._verbose         = parsed isSet "--verbose"
    fileManager.showDiff    = true
    // parsed isSet "--show-diff"
    fileManager.updateCheck = parsed isSet "--update-check"
    fileManager.showLog     = parsed isSet "--show-log"
    fileManager.failed      = parsed isSet "--failed"

    if (parsed isSet "--ansi") NestUI initialize NestUI.MANY
    if (parsed isSet "--timeout") fileManager.timeout = parsed("--timeout")
    if (parsed isSet "--debug") setProp("partest.debug", "true")

    def addTestFile(file: File) = {
      if (!file.exists)
        NestUI.failure("Test file '%s' not found, skipping.\n" format file)
      else {
        NestUI.verbose("adding test file " + file)
        testFiles +:= file
      }
    }

    // If --grep is given we suck in every file it matches.

    val grepOption = parsed get "--grep"
    val grepPaths = grepOption.toList flatMap { expr =>
      val subjectDirs = testSetKinds map (srcDir / _ toDirectory)
      val testPaths   = subjectDirs flatMap (_.files filter stdFilter)
      val paths       = testPaths filter (p => pathMatchesExpr(p, expr))

      if (paths.isEmpty)
         NestUI.failure("--grep string '%s' matched no tests." format expr)

      paths map (_.jfile)
    }
    val grepMessage = grepOption map (x => "Argument '%s' matched %d test(s)".format(x, grepPaths.size)) getOrElse ""

    grepPaths foreach addTestFile
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
        fileManager.latestCompFile.getParentFile.getParentFile.getAbsoluteFile
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
    ) foreach (x => NestUI verbose (x + "\n"))

    NestUI.verbose("available processors: " + Runtime.getRuntime().availableProcessors())

    // Dragged down here so it isn't buried under the banner.
    if (grepMessage != "")
      NestUI.normal(grepMessage + "\n")

    val ((successes, failures), elapsedMillis) = timed(testCheckAll(enabledTestSets))
    val total = successes + failures

    val elapsedSecs = elapsedMillis/1000
    val elapsedMins = elapsedSecs/60
    val elapsedHrs  = elapsedMins/60
    val dispMins = elapsedMins - elapsedHrs  * 60
    val dispSecs = elapsedSecs - elapsedMins * 60

    val dispElapsed = {
      def form(num: Long) = if (num < 10) "0"+num else ""+num
      form(elapsedHrs)+":"+form(dispMins)+":"+form(dispSecs)
    }

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
    def kindOf(f: File) = {
      (srcDir relativize Path(f).toCanonical).segments match {
        case (".." :: "scaladoc" :: xs) => xs.head
        case xs => xs.head
      }
    }

    val (valid, invalid) = testFiles partition (x => testSetKinds contains kindOf(x))
    invalid foreach (x => NestUI.failure(
      "Invalid test file '%s', skipping.\n".format(x) +
      "(Test kind '%s' not in known set '%s')".format(kindOf(x), testSetKinds))
    )

    val grouped = (valid groupBy kindOf).toList sortBy (x => testSetKinds indexOf x._1)
    val runTestsFileLists =
      for ((kind, files) <- grouped) yield {
        NestUI.outline("\nTesting individual files\n")
        resultsToStatistics(runTestsForFiles(files, kind))
      }

    if (enabledSets.nonEmpty)
      NestUI.verbose("Run sets: "+enabledSets)

    val results = runTestsFileLists ::: (enabledSets map runTests)

    (results map (_._1) sum, results map (_._2) sum)
  }
}
