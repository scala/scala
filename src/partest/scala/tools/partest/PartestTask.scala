/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest

import scala.util.Properties.setProp
import scala.tools.ant.sabbus.CompilationPathProperty
import java.lang.reflect.Method
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{ Reference, FileSet}
import org.apache.tools.ant.types.Commandline.Argument

/** An Ant task to execute the Scala test suite (NSC).
 *
 *  This task can take the following parameters as attributes:
 *  - `srcdir`,
 *  - `classpath`,
 *  - `classpathref`,
 *  - `erroronfailed`,
 *  - `javacmd`,
 *  - `javaccmd`,
 *  - `scalacopts`,
 *  - `debug`,
 *  - `junitreportdir`.
 *
 *  It also takes the following parameters as nested elements:
 *  - `compilationpath`.
 *
 * @author Philippe Haller
 */
class PartestTask extends Task with CompilationPathProperty {
  type Path = org.apache.tools.ant.types.Path

  private var kinds: List[String]               = Nil
  private var classpath: Option[Path]           = None
  private var debug                             = false
  private var errorOnFailed: Boolean            = true
  private var jUnitReportDir: Option[File]      = None
  private var javaccmd: Option[File]            = None
  private var javacmd: Option[File]             = Option(sys.props("java.home")) map (x => new File(x, "bin/java"))
  private var scalacArgs: Option[Seq[Argument]] = None
  private var srcDir: Option[String]            = None
  private var colors: Int = 0

  def setSrcDir(input: String) {
    srcDir = Some(input)
  }

  def setColors(input: String) {
    try colors = input.toInt catch { case _: NumberFormatException => () }
    if (colors > 0)
      sys.props("partest.colors") = colors.toString
  }

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

  def setClasspathref(input: Reference) {
    createClasspath().setRefid(input)
  }
  def setErrorOnFailed(input: Boolean) {
    errorOnFailed = input
  }

  def setJavaCmd(input: File) {
    javacmd = Some(input)
  }

  def setKinds(input: String) {
    kinds = words(input)
  }

  def setJavacCmd(input: File) {
    javaccmd = Some(input)
  }

  def setScalacOpts(input: String) {
    val s = input.split(' ').map { s => val a = new Argument; a.setValue(s); a }
    scalacArgs = Some(scalacArgs.getOrElse(Seq()) ++ s)
  }

  def createCompilerArg(): Argument = {
    val a = new Argument
    scalacArgs = Some(scalacArgs.getOrElse(Seq()) :+ a)
    a
  }

  def setDebug(input: Boolean) {
    debug = input
  }

  def setJUnitReportDir(input: File) {
    jUnitReportDir = Some(input)
  }

  override def execute() {
    if (debug || sys.props.contains("partest.debug")) {
      nest.NestUI.setDebug()
    }

    srcDir foreach (x => setProp("partest.srcdir", x))

    val classpath = this.compilationPath getOrElse sys.error("Mandatory attribute 'compilationPath' is not set.")
    val cpfiles = classpath.list map { fs => new File(fs) } toList
    def findCp(name: String) = cpfiles find (f =>
         (f.getName == s"scala-$name.jar")
      || (f.absolutePathSegments endsWith Seq("classes", name))
    ) getOrElse sys.error(s"Provided classpath does not contain a Scala $name element.")

    val scalaLibrary         = findCp("library")
    val scalaReflect         = findCp("reflect")
    val scalaCompiler        = findCp("compiler")
    val scalaPartest         = findCp("partest")
    val scalaActors          = findCp("actors")

    def scalacArgsFlat: Option[Seq[String]] = scalacArgs map (_ flatMap { a =>
      val parts = a.getParts
      if (parts eq null) Nil else parts.toSeq
    })

    val antRunner = new scala.tools.partest.nest.AntRunner
    val antFileManager = antRunner.fileManager

    // antFileManager.failed = runFailed
    antFileManager.CLASSPATH = ClassPath.join(classpath.list: _*)
    antFileManager.LATEST_LIB = scalaLibrary.getAbsolutePath
    antFileManager.LATEST_REFLECT = scalaReflect.getAbsolutePath
    antFileManager.LATEST_COMP = scalaCompiler.getAbsolutePath
    antFileManager.LATEST_PARTEST = scalaPartest.getAbsolutePath
    antFileManager.LATEST_ACTORS = scalaActors.getAbsolutePath

    javacmd foreach (x => antFileManager.JAVACMD = x.getAbsolutePath)
    javaccmd foreach (x => antFileManager.JAVAC_CMD = x.getAbsolutePath)
    scalacArgsFlat foreach (antFileManager.SCALAC_OPTS ++= _)

    def runSet(kind: String, files: Array[File]): (Int, Int, List[String]) = {
      if (files.isEmpty) (0, 0, List())
      else {
        log(s"Running ${files.length} tests in '$kind' at $now")
        // log(s"Tests: ${files.toList}")
        val results = antRunner.reflectiveRunTestsForFiles(files, kind)
        val (passed, failed) = results partition (_.isOk)
        val numPassed = passed.size
        val numFailed = failed.size
        def failedMessages = failed map (_.longStatus)

        log(s"Completed '$kind' at $now")

        // create JUnit Report xml files if directory was specified
        jUnitReportDir foreach { d =>
          d.mkdir

          val report = testReport(kind, results, numPassed, numFailed)
          scala.xml.XML.save(d.getAbsolutePath+"/"+kind+".xml", report)
        }

        (numPassed, numFailed, failedMessages)
      }
    }

    val _results       = kinds map (k => runSet(k, TestKinds testsFor k map (_.jfile) toArray))
    val allSuccesses   = _results map (_._1) sum
    val allFailures    = _results map (_._2) sum
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

  private def oneResult(res: TestState) =
    <testcase name={res.testIdent}>{
      if (res.isOk) scala.xml.NodeSeq.Empty
      else <failure message="Test failed"/>
    }</testcase>

  private def testReport(kind: String, results: Iterable[TestState], succs: Int, fails: Int) =
    <testsuite name={kind} tests={(succs + fails).toString} failures={fails.toString}>
      <properties/>
      {
        results map oneResult
      }
    </testsuite>
}
