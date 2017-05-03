/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
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
