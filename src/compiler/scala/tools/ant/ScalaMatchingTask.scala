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

import org.apache.tools.ant.{ Task, BuildException }
import org.apache.tools.ant.taskdefs.MatchingTask

trait ScalaTask {
  self: Task =>

  /** Generates a build error. Error location will be the
   *  current task in the ant file.
   *
   * @param message A message describing the error.
   * @throws BuildException A build error exception thrown in every case.
   */
   protected def buildError(message: String): Nothing =
     throw new BuildException(message, getLocation())
}

abstract class ScalaMatchingTask extends MatchingTask with ScalaTask
