/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest

import scala.actors.Actor._
import scala.util.Properties.setProp

import ant.sabbus.CompilationPathProperty
import nsc.io.{ Directory, Path => SPath }
import nsc.util.ClassPath
import util.PathResolver

import java.io.File
import java.lang.reflect.Method

import org.apache.tools.ant.{BuildException, Task}
import org.apache.tools.ant.types.{Path, Reference, FileSet}

/** An Ant task to execute the Scala test suite (NSC).
 *
 *  This task can take the following parameters as attributes:
 *  - `srcdir`,
 *  - `classpath`,
 *  - `classpathref`,
 *  - `showlog`,
 *  - `showdiff`,
 *  - `erroronfailed`,
 *  - `javacmd`,
 *  - `javaccmd`,
 *  - `scalacopts`,
 *  - `timeout`,
 *  - `debug`,
 *  - `junitreportdir`.
 *
 *  It also takes the following parameters as nested elements:
 *  - `compilationpath`.
 *  - `postests`,
 *  - `negtests`,
 *  - `runtests`,
 *  - `jvmtests`,
 *  - `residenttests`,
 *  - `buildmanagertests`,
 *  - `shootouttests`,
 *  - `scalaptests`,
 *  - `scalachecktests`,
 *  - `specializedtests`,
 *  - `presentationtests`,
 *  - `scripttests`.
 *
 * @author Philippe Haller, Stephane Micheloud
 */
class PartestTask extends Task with CompilationPathProperty {

  def addConfiguredPosTests(input: FileSet) {
    posFiles = Some(input)
  }

  def addConfiguredNegTests(input: FileSet) {
    negFiles = Some(input)
  }

  def addConfiguredRunTests(input: FileSet) {
    runFiles = Some(input)
  }

  def addConfiguredJvmTests(input: FileSet) {
    jvmFiles = Some(input)
  }

  def addConfiguredResidentTests(input: FileSet) {
    residentFiles = Some(input)
  }

  def addConfiguredBuildManagerTests(input: FileSet) {
    buildManagerFiles = Some(input)
  }

  def addConfiguredScalacheckTests(input: FileSet) {
    scalacheckFiles = Some(input)
  }

  def addConfiguredScriptTests(input: FileSet) {
    scriptFiles = Some(input)
  }

  def addConfiguredShootoutTests(input: FileSet) {
    shootoutFiles = Some(input)
  }

  def addConfiguredScalapTests(input: FileSet) {
    scalapFiles = Some(input)
  }

  def addConfiguredSpecializedTests(input: FileSet) {
    specializedFiles = Some(input)
  }

  def addConfiguredPresentationTests(input: FileSet) {
    presentationFiles = Some(input)
  }

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the `srcdir` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `srcdir`. */
  def setSrcDir(input: String) {
    srcDir = Some(input)
  }

  /** Sets the `classpath` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `classpath`. */
  def setClasspath(input: Path) {
    if (classpath.isEmpty)
      classpath = Some(input)
    else
      classpath.get.append(input)
  }

  def createClasspath(): Path = {
    if (classpath.isEmpty) classpath = Some(new Path(getProject()))
    classpath.get.createPath()
  }

  /** Sets the `classpathref` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `classpathref`. */
  def setClasspathref(input: Reference) {
    createClasspath().setRefid(input)
  }

  /** Sets the `showlog` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `showlog`. */
  def setShowLog(input: Boolean) {
    showLog = input
  }

  /** Sets the `showdiff` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `showdiff`. */
  def setShowDiff(input: Boolean) {
    showDiff = input
  }

  /** Sets the `erroronfailed` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `erroronfailed`. */
  def setErrorOnFailed(input: Boolean) {
    errorOnFailed = input
  }

  /** Sets the `javacmd` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `javacmd`. */
  def setJavaCmd(input: File) {
    javacmd = Some(input)
  }

  /** Sets the `javaccmd` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `javaccmd`. */
  def setJavacCmd(input: File) {
    javaccmd = Some(input)
  }

  /** Sets the `scalacopts` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `scalacopts`. */
  def setScalacOpts(opts: String) {
    scalacOpts = Some(opts)
  }

  /** Sets the `timeout` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `timeout`. */
  def setTimeout(delay: String) {
    timeout = Some(delay)
  }

  /** Sets the `debug` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `debug`. */
  def setDebug(input: Boolean) {
    debug = input
  }

  /** Sets the `junitreportdir` attribute. Used by [[http://ant.apache.org Ant]].
   *  @param input The value of `junitreportdir`. */
  def setJUnitReportDir(input: File) {
    jUnitReportDir = Some(input)
  }

  /** The class path to use for this compilation. */
  private var classpath: Option[Path] = None
  private var srcDir: Option[String] = None
  private var javacmd: Option[File] = None
  private var javaccmd: Option[File] = None
  private var showDiff: Boolean = false
  private var showLog: Boolean = false
  private var runFailed: Boolean = false
  private var posFiles: Option[FileSet] = None
  private var negFiles: Option[FileSet] = None
  private var runFiles: Option[FileSet] = None
  private var jvmFiles: Option[FileSet] = None
  private var residentFiles: Option[FileSet] = None
  private var buildManagerFiles: Option[FileSet] = None
  private var scalacheckFiles: Option[FileSet] = None
  private var scriptFiles: Option[FileSet] = None
  private var shootoutFiles: Option[FileSet] = None
  private var scalapFiles: Option[FileSet] = None
  private var specializedFiles: Option[FileSet] = None
  private var presentationFiles: Option[FileSet] = None
  private var errorOnFailed: Boolean = false
  private var scalacOpts: Option[String] = None
  private var timeout: Option[String] = None
  private var jUnitReportDir: Option[File] = None
  private var debug = false

  private def fileSetToDir(fs: FileSet) = Directory(fs getDir getProject)

  private def getFiles(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None =>
      Array()
    case Some(fs) =>
      val sFiles: Array[SPath] = {
        val root = fileSetToDir(fs)
        (fs getDirectoryScanner getProject).getIncludedFiles map (root / _)
      }
      def shouldExclude(p: SPath) = (p hasExtension "log") || (p.name startsWith ".")
      sFiles filterNot (shouldExclude(_)) map (_.jfile)
  }

  private def getDirs(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None =>
      Array()
    case Some(fs) =>
      val sDirectories: Array[SPath] = {
        val root = fileSetToDir(fs)
        (fs getDirectoryScanner getProject).getIncludedDirectories map (root / _)
      }
      def shouldExclude(p: SPath) = (p hasExtension "obj") || (p.name startsWith ".")
      sDirectories filterNot (shouldExclude(_)) map (_.jfile)
  }

  private def getFilesAndDirs(fileSet: Option[FileSet]): Array[File] = fileSet match {
    case None     => Array()
    case Some(fs) => getFiles(fileSet) ++ getDirs(fileSet)
  }

  private def getPosFiles          = getFilesAndDirs(posFiles)
  private def getNegFiles          = getFilesAndDirs(negFiles)
  private def getRunFiles          = getFilesAndDirs(runFiles)
  private def getJvmFiles          = getFilesAndDirs(jvmFiles)
  private def getResidentFiles     = getFiles(residentFiles)
  private def getBuildManagerFiles = getFilesAndDirs(buildManagerFiles)
  private def getScalacheckFiles   = getFilesAndDirs(scalacheckFiles)
  private def getScriptFiles       = getFiles(scriptFiles)
  private def getShootoutFiles     = getFiles(shootoutFiles)
  private def getScalapFiles       = getFiles(scalapFiles)
  private def getSpecializedFiles  = getFiles(specializedFiles)
  private def getPresentationFiles = getDirs(presentationFiles)

  protected def initialize() {
    if (isPartestDebug || debug) {
      setProp("partest.debug", "true")
      nest.NestUI._verbose = true
    }

    // Tests if all mandatory attributes are set and valid.
    //if (srcDir.isEmpty)
    //  throw new BuildException("Attribute 'srcdir' is not set.", getLocation)
  }

  override def execute() {
    initialize()

    srcDir foreach (x => setProp("partest.srcdir", x))

    val classpath = this.compilationPath getOrElse sys.error("Mandatory attribute 'compilationPath' is not set.")

    val scalaLibrary = {
      (classpath.list map { fs => new File(fs) }) find { f =>
        f.getName match {
          case "scala-library.jar" => true
          case "library" if (f.getParentFile.getName == "classes") => true
          case _ => false
        }
      }
    } getOrElse sys.error("Provided classpath does not contain a Scala library.")

    val antRunner = new scala.tools.partest.nest.AntRunner
    val antFileManager = antRunner.fileManager

    antFileManager.showDiff = showDiff
    antFileManager.showLog = showLog
    antFileManager.failed = runFailed
    antFileManager.CLASSPATH = ClassPath.join(classpath.list: _*)
    antFileManager.LATEST_LIB = scalaLibrary.getAbsolutePath

    javacmd foreach (x => antFileManager.JAVACMD = x.getAbsolutePath)
    javaccmd foreach (x => antFileManager.JAVAC_CMD = x.getAbsolutePath)
    scalacOpts foreach (antFileManager.SCALAC_OPTS = _)
    timeout foreach (antFileManager.timeout = _)

    type TFSet = (Array[File], String, String)
    val testFileSets = List(
      (getPosFiles, "pos", "Compiling files that are expected to build"),
      (getNegFiles, "neg", "Compiling files that are expected to fail"),
      (getRunFiles, "run", "Compiling and running files"),
      (getJvmFiles, "jvm", "Compiling and running files"),
      (getResidentFiles, "res", "Running resident compiler scenarii"),
      (getBuildManagerFiles, "buildmanager", "Running Build Manager scenarii"),
      (getScalacheckFiles, "scalacheck", "Running scalacheck tests"),
      (getScriptFiles, "script", "Running script files"),
      (getShootoutFiles, "shootout", "Running shootout tests"),
      (getScalapFiles, "scalap", "Running scalap tests"),
      (getSpecializedFiles, "specialized", "Running specialized files"),
      (getPresentationFiles, "presentation", "Running presentation compiler test files")
    )

    def runSet(set: TFSet): (Int, Int, Iterable[String]) = {
      val (files, name, msg) = set
      if (files.isEmpty) (0, 0, List())
      else {
        log(msg)
        val results: Iterable[(String, Int)] = antRunner.reflectiveRunTestsForFiles(files, name)
        val (succs, fails) = resultsToStatistics(results)

        val failed: Iterable[String] = results collect {
          case (path, 1) => path + " [FAILED]"
          case (path, 2) => path + " [TIMOUT]"
        }

        // create JUnit Report xml files if directory was specified
        jUnitReportDir foreach { d =>
          d.mkdir()

          val report = testReport(name, results, succs, fails)
/*@XML*/
          scala.xml.XML.save(d.getAbsolutePath+"/"+name+".xml", report)
/*XML@*/
/*@NOXML
          util.XML.save(d.getAbsolutePath+"/"+name+".xml", report)
XMLNO@*/
        }

        (succs, fails, failed)
      }
    }

    val _results = testFileSets map runSet
    val allSuccesses = _results map (_._1) sum
    val allFailures = _results map (_._2) sum
    val allFailedPaths = _results flatMap (_._3)

    def f = if (errorOnFailed && allFailures > 0) (sys error _) else log(_: String)
    def s = if (allFailures > 1) "s" else ""
    val msg =
      if (allFailures > 0)
        "Test suite finished with %d case%s failing:\n".format(allFailures, s)+
        allFailedPaths.mkString("\n")
      else if (allSuccesses == 0) "There were no tests to run."
      else "Test suite finished with no failures."

    f(msg)
  }
/*@XML*/ // NB. This code DOES rely on Scala native XML support.
  def oneResult(res: (String, Int)) =
    <testcase name={res._1}>{
  	  res._2 match {
  	    case 0 => scala.xml.NodeSeq.Empty
        case 1 => <failure message="Test failed"/>
        case 2 => <failure message="Test timed out"/>
  	  }
  	}</testcase>

  def testReport(kind: String, results: Iterable[(String, Int)], succs: Int, fails: Int) =
    <testsuite name={kind} tests={(succs + fails).toString} failures={fails.toString}>
  	  <properties/>
  	  {
  	    results.map(oneResult(_))
  	  }
    </testsuite>
/*XML@*/
/*@NOXML // NB. This code DOES NOT rely on Scala native XML support.
  private def testReport(kind: String, results: Iterable[(String, Int)], succs: Int, fails: Int) = {
    val root = util.XML.newDocument()

    def testCase(res: (String, Int)) = {
      val testcaseElem = root createElement "testcase"
      testcaseElem.setAttribute("name", res._1)
      val text = res._2 match {
        case 0 => null
        case 1 => "Test failed"
        case 2 => "Test timed out"
      }
      if (text != null) {
        val failureElem = root createElement "failure"
        failureElem.setAttribute("message", text)
        testcaseElem appendChild failureElem
      }
      testcaseElem
    }

    val testsuiteElem = root createElement "testsuite"
    testsuiteElem.setAttribute("name", kind)
    testsuiteElem.setAttribute("tests", (succs+fails).toString)
    testsuiteElem.setAttribute("failures", fails.toString)
    root appendChild testsuiteElem

    testsuiteElem appendChild (root createElement "properties")
    results foreach (res => testsuiteElem appendChild testCase(res))

    root
  }
XMLNO@*/
}
