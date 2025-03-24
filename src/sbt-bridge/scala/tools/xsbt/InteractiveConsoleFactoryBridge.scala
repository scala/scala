/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Scala Center, Lightbend dba Akka, and Mark Harrah
 *
 * Scala (https://www.scala-lang.org)
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package xsbt

import java.util.Optional
import xsbti.Logger

class InteractiveConsoleBridgeFactory extends xsbti.InteractiveConsoleFactory {
  def createConsole(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      initialCommands: String,
      cleanupCommands: String,
      loader: Optional[ClassLoader],
      bindNames: Array[String],
      bindValues: Array[AnyRef],
      log: Logger
  ): xsbti.InteractiveConsoleInterface =
    new InteractiveConsoleBridge(
      args,
      bootClasspathString,
      classpathString,
      initialCommands,
      cleanupCommands,
      loader,
      bindNames,
      bindValues,
      log
    )
}
