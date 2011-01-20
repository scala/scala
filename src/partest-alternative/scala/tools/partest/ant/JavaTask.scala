/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest
package ant

import org.apache.tools.ant.Task
import org.apache.tools.ant.taskdefs.Java
import org.apache.tools.ant.types.Environment

import scala.tools.nsc.io._
import scala.tools.nsc.util.ClassPath
import cmd.Spec._

class JavaTask extends Java {
  override def getTaskName()      = "partest"
  private val scalaRunnerClass    = "scala.tools.nsc.MainGenericRunner"
  private val partestRunnerClass  = "scala.tools.partest.Runner"
  def defaultJvmArgs              = "-Xms64M -Xmx768M -Xss768K -XX:MaxPermSize=96M"

  protected def rootDir           = prop("partest.rootdir") getOrElse (baseDir / "test").path
  protected def partestJVMArgs    = prop("partest.jvm.args") getOrElse defaultJvmArgs
  protected def runnerArgs        = List("-usejavacp", partestRunnerClass, "--javaopts", partestJVMArgs)

  private def baseDir             = Directory(getProject.getBaseDir)
  private def prop(s: String)     = Option(getProject getProperty s)
  private def jvmline(s: String)  = returning(createJvmarg())(_ setLine s)
  private def addArg(s: String)   = returning(createArg())(_ setValue s)

  private def newKeyValue(key: String, value: String) =
    returning(new Environment.Variable)(x => { x setKey key ; x setValue value })

  def setDefaults() {
    setFork(true)
    setFailonerror(true)
    getProject.setSystemProperties()
    setClassname(scalaRunnerClass)
    addSysproperty(newKeyValue("partest.is-in-ant", "true"))
    jvmline(partestJVMArgs)
    runnerArgs foreach addArg

    // do we want basedir or rootDir to be the cwd?
    // setDir(Path(rootDir).jfile)
  }

  override def init() = {
    super.init()
    setDefaults()
  }
}

