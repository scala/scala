/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

/**** Note -- this isn't used anymore, but I left it in for the moment. ****/

package scala.tools
package partest
package ant

import java.io.{ File => JFile }

import org.apache.tools.ant.Task
import org.apache.tools.ant.types.{ Reference, FileSet}

import scala.reflect.BeanProperty
import scala.tools.ant.sabbus.CompilationPathProperty
import scala.tools.nsc.io
import scala.tools.nsc.util.CommandLineSpec._

class PartestTask extends Task with CompilationPathProperty {
  /** Used only in ant task */
  @BeanProperty protected var errorOnFailed: Boolean = _
  @BeanProperty protected var jUnitReportDir: JFile = _

  /** Propagated to partest run via system properties */
  @BeanProperty protected var debug: Boolean = _
  @BeanProperty protected var javaOpts: String = _
  @BeanProperty protected var partestOpts: String = _
  @BeanProperty protected var runSets: String = _
  @BeanProperty protected var scalacOpts: String = _
  @BeanProperty protected var showDiff: Boolean = _
  @BeanProperty protected var showLog: Boolean = _
  @BeanProperty protected var srcDir: String = _
  @BeanProperty protected var timeout: Int = _

  /** Translating ant information into command line arguments. */
  private def notEmpty(s: String) = s != null && s.length > 0
  private def quoted(s: String)   = if (s exists (_.isWhitespace)) "\"" + s.trim + "\"" else s
  private def optionCollection = List[(Boolean, () => List[String])](
    debug                 -> (() => List("--debug")),
    showLog               -> (() => List("--show-log")),
    showDiff              -> (() => List("--show-diff")),
    (timeout > 0)         -> (() => List("--timeout", timeout.toString)),
    notEmpty(javaOpts)    -> (() => List("--javaopts", javaOpts)),
    notEmpty(scalacOpts)  -> (() => List("--scalacopts", scalacOpts)),
    notEmpty(srcDir)      -> (() => List("--srcdir", srcDir)),
    notEmpty(partestOpts) -> (() => toArgs(partestOpts))
  )

  private def antPropOrNone(name: String) = Option(getProject getProperty name)
  private def antPropsToCommandLine() = {
    setProp("partest.isInAnt", "true")
    val partestDir = antPropOrNone("partest.dir") getOrElse error("Mandatory attribute 'partest.dir' is not set.")

    val root = List("--rootdir", io.Path(partestDir).path)
    val opts = optionCollection collect { case (true, f)  => f() } flatten
    val sets = Option(runSets).toList flatMap toArgs map toOpt

    root ++ opts ++ sets
  }
  private def antRunTests() = {
    val args    = antPropsToCommandLine()
    val runner  = Partest(args: _*)
    import runner._

    normal("Ant options translate to command line: partest " + fromArgs(args))
    printConfigBanner()

    val result  = launchTestSuite()
    val msg     = result.toString

    if (result.hasFailures && errorOnFailed) error(msg)
    else log(msg)
  }

  override def execute() {
    try antRunTests()
    catch {
      case x  =>
        System.err.println("Uncaught exception %s in partest ant ask: aborting." format x)
        x.printStackTrace()
        throw x
    }
  }
}
