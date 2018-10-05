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

package scala.tools.partest
package nest

object ConsoleRunner {
  def main(args: Array[String]): Unit = {
    val config = RunnerSpec.forArgs(args)
    val r = new AbstractRunner(
      config,
      config.optSourcePath getOrElse PartestDefaults.sourcePath,
      new FileManager(ClassPath split PathResolver.Environment.javaUserClassPath map (Path(_))) // the script sets up our classpath for us via ant
    )
    // So we can ctrl-C a test run and still hear all
    // the buffered failure info.
    scala.sys addShutdownHook r.issueSummaryReport()
    System.exit( if (r.run) 0 else 1 )
  }
}
