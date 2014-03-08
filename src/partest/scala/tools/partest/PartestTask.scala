/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest

import scala.tools.nsc.Properties.propOrFalse
import scala.tools.ant.sabbus.CompilationPathProperty
import java.lang.reflect.Method
import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{ Reference, FileSet }
import org.apache.tools.ant.types.Commandline.Argument
import scala.tools.ant.ScalaTask
import nest.NestUI
import java.net.URLClassLoader

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
 *  - `compilationpath`. -- TODO: this parameter is now redundant: it's the same as the classpath used to run the task
 *
 *  @author Philipp Haller, Adriaan Moors
 */
class PartestTask extends Task with CompilationPathProperty with ScalaTask {
  type Path = org.apache.tools.ant.types.Path

  private var kinds: Array[String] = Array.empty
  private var classpath: Option[Path] = None
  private var debug = false
  private var errorOnFailed: Boolean = true
  private var jUnitReportDir: Option[File] = None
  private var javaccmd: Option[File] = None
  private var javacmd: Option[File] = Option(sys.props("java.home")) map (x => new File(x, "bin/java"))
  private var scalacArgs: Option[Seq[Argument]] = None
  private var srcDir: Option[String] = None
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
    kinds = words(input).toArray
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
    if (debug || propOrFalse("partest.debug")) {
      NestUI.setDebug()
    }

    if (compilationPath.isEmpty) sys.error("Mandatory attribute 'compilationPath' is not set.")

    def scalacArgsFlat: Array[String] = scalacArgs.toArray flatMap (_ flatMap { a =>
      val parts = a.getParts
      if (parts eq null) Array.empty[String] else parts
    })

    var failureCount = 0
    val summary = new scala.tools.partest.nest.AntRunner(srcDir.getOrElse(null), new URLClassLoader(compilationPath.get.list.map(Path(_).toURL)), javacmd.getOrElse(null), javaccmd.getOrElse(null), scalacArgsFlat) {
      def echo(msg: String): Unit = PartestTask.this.log(msg)
      def log(msg: String): Unit = PartestTask.this.log(msg)

      def onFinishKind(kind: String, passed: Array[TestState], failed: Array[TestState]) = {
        failureCount += failed.size

        def oneResult(res: TestState) =
          <testcase name={ res.testIdent }>{
            if (res.isOk) scala.xml.NodeSeq.Empty
            else <failure message="Test failed"/>
          }</testcase>

        // create JUnit Report xml files if directory was specified
        jUnitReportDir foreach { d =>
          d.mkdir
          val report =
            <testsuite name={ kind } tests={ (passed.size + failed.size) toString } failures={ failed.size.toString }>
              <properties/>{ passed map oneResult }{ failed map oneResult }
            </testsuite>

          scala.xml.XML.save(d.getAbsolutePath + "/" + kind + ".xml", report)
        }
      }
    } execute (kinds)

    if (errorOnFailed && failureCount > 0) buildError(summary)
    else log(summary)

  }
}
