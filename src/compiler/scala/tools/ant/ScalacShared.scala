/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

import org.apache.tools.ant.Project
import org.apache.tools.ant.taskdefs.Java
import scala.tools.nsc.io

trait ScalacShared extends ScalaMatchingTask {
  val MainClass = "scala.tools.nsc.Main"

  def execWithArgFiles(java: Java, paths: List[String]) = {
    paths foreach (p => java.createArg() setValue ("@"+ p))

    val debugString = paths map (x => " (@ = '%s')".format(io.File(x).slurp())) mkString ""
    log(java.getCommandLine.getCommandline.mkString("", " ", debugString), Project.MSG_VERBOSE)
    java.executeJava()
  }
}
