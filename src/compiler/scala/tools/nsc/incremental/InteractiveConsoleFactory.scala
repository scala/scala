/*
 * Zinc - The incremental compiler for Scala.
 * Copyright Lightbend, Inc. and Mark Harrah
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package xsbt

import xsbti.Logger

class InteractiveConsoleFactory extends xsbti.InteractiveConsoleFactory {
  def createConsole(
      args: Array[String],
      bootClasspathString: String,
      classpathString: String,
      initialCommands: String,
      cleanupCommands: String,
      loader: ClassLoader,
      bindNames: Array[String],
      bindValues: Array[AnyRef],
      log: Logger
  ): xsbti.InteractiveConsoleInterface =
    new InteractiveConsoleInterface(
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
