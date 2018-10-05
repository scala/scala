/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
