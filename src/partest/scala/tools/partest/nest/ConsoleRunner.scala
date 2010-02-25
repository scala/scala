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
import scala.tools.nsc.interpreter.returning
import io.{ Path }

class ConsoleRunner extends DirectRunner {

  case class TestSet(loc: String,
                     filter: Option[(String, Boolean)],
                     kind: String,
                     msg: String)

  val testSets = {
    val fileFilter = Some((".scala", true))
    List(
      TestSet("pos", fileFilter, "pos",
              "Testing compiler (on files whose compilation should succeed)"),
      TestSet("neg", fileFilter, "neg",
              "Testing compiler (on files whose compilation should fail)"),
      TestSet("run", fileFilter, "run", "Testing JVM backend"),
      TestSet("jvm", fileFilter, "jvm", "Testing JVM backend"),
      TestSet("res", Some((".res", false)), "res",
              "Testing resident compiler"),
      TestSet("buildmanager", Some((".nothing", true)), "buildmanager", "Testing Build Manager"),
      TestSet("shootout", fileFilter, "shootout", "Testing shootout tests"),
      TestSet("script", fileFilter, "script", "Testing script tests"),
      TestSet("scalacheck", fileFilter, "scalacheck", "Testing ScalaCheck tests"),
      TestSet("scalap", fileFilter, "scalap", "Run scalap decompiler tests"))
  }

  var fileManager: ConsoleFileManager = _

  private var testFiles: List[File] = List()
  private val errors = PartestDefaults.errorCount

  private val testSetArgMap = testSets map (x => ("--" + x.loc) -> x) toMap
  private val testSetArgs = testSets map ("--" + _.loc)
  def denotesTestSet(arg: String) = testSetArgs contains arg
  def denotesTestFile(arg: String) = (arg endsWith ".scala") || (arg endsWith ".res")
  def denotesTestDir(arg: String) = Path(arg).isDirectory
  def denotesTestPath(arg: String) = denotesTestDir(arg) || denotesTestFile(arg)

  private def printVersion { NestUI outline (versionMsg + "\n") }

  private val unaryArgs = List(
    "--pack", "--all", "--verbose", "--show-diff", "--show-log",
    "--failed", "--version", "--ansi", "--debug"
  ) ::: testSetArgs

  def main(argstr: String) {
    val parsed = CommandLineParser(argstr) withUnaryArguments unaryArgs
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
    val enabledTestSets = {
      val enabledArgs = testSetArgs filter parsed.isSet

      if (args.isEmpty && (enabledArgs.isEmpty || (parsed isSet "--all"))) testSets
      else enabledArgs map testSetArgMap
    }

    NestUI._verbose       = parsed isSet "--verbose"
    fileManager.showDiff  = parsed isSet "--show-diff"
    fileManager.showLog   = parsed isSet "--show-log"
    fileManager.failed    = parsed isSet "--failed"

    if (parsed isSet "--ansi") NestUI initialize NestUI.MANY
    if (parsed isSet "--timeout") fileManager.timeout = parsed("--timeout")
    if (parsed isSet "--debug") setProp("partest.debug", "true")

    testFiles :::= args map { arg =>
      val file = new File(arg)
      if (!file.exists) {
        NestUI.failure("File \"%s\" not found\n" format arg)
        System.exit(1)
      }

      returning[File](file)(x => NestUI.verbose("adding test file " + x))
    }

    val dir =
      if (fileManager.testClasses.isDefined) fileManager.testClassesDir
      else fileManager.testBuildFile getOrElse {
        fileManager.latestCompFile.getParentFile.getParentFile.getCanonicalFile
      }

    val vmBin  = javaHome + File.separator + "bin"
    val vmName = "%s (build %s, %s)".format(javaVmName, javaVmVersion, javaVmInfo)
    val vmOpts = fileManager.JAVA_OPTS

    NestUI.verbose("enabled test sets: " + (enabledTestSets map (_.loc) mkString " "))

    List(
      "Scala compiler classes in: " + dir,
      "Scala version is:          " + versionMsg,
      "Scalac options are:        " + fileManager.SCALAC_OPTS,
      "Java binaries in:          " + vmBin,
      "Java runtime is:           " + vmName,
      "Java options are:          " + vmOpts,
      "Source directory is:       " + fileManager.srcDir.path
    ) foreach (x => NestUI outline (x + "\n"))

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
    val TestSet(loc, filter, kind, msg) = testSet

    fileManager.getFiles(loc, true, filter) match {
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
    def runTestsFiles = if (!testFiles.isEmpty) {
      def absName(f: File): String = f.getAbsoluteFile.getCanonicalPath

      def kindOf(f: File): String = {
        val firstName = absName(f)
        val len = fileManager.srcDirName.length
        val filesPos = firstName.indexOf(fileManager.srcDirName)
        if (filesPos == -1) {
          NestUI.failure("invalid test file: "+firstName+"\n")
          Predef.exit(1)
        } else {
          val short = firstName drop (filesPos + len + 1) take 3
          val shortKinds = List("pos", "neg", "run", "jvm", "res")
          if (shortKinds contains short) short
          else short match {
            case "sho" => "shootout"
            case "scr" => "script"
            case "sca" => "scalacheck"
            case "bui" => "buildmanager"
          }
        }
      }

      val fstKind = kindOf(testFiles.head)
      NestUI.verbose("all test files expected to have kind "+fstKind)
      if (!testFiles.forall(kindOf(_) equals fstKind)) {
        NestUI.failure("test files have different kinds\n")
        Predef.exit(1)
      } else {
        NestUI.outline("\nTesting individual files\n")
        resultsToStatistics(runTestsForFiles(testFiles, fstKind))
      }
    } else (0, 0)

    NestUI.verbose("run sets: "+enabledSets)

    val results = List(runTestsFiles) ::: (enabledSets map runTests)
    results reduceLeft { (p: (Int, Int), q: (Int, Int)) =>
      (p._1+q._1, p._2+q._2) }
  }
}
