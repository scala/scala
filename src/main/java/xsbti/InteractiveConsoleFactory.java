/*
 * Zinc - The incremental compiler for Scala.
 * Copyright 2011 - 2017, Lightbend, Inc.
 * Copyright 2008 - 2010, Mark Harrah
 * This software is released under the terms written in LICENSE.
 */

package xsbti;

public interface InteractiveConsoleFactory {
  InteractiveConsoleInterface createConsole(
      String[] args,
      String bootClasspathString,
      String classpathString,
      String initialCommands,
      String cleanupCommands,
      ClassLoader loader,
      String[] bindNames,
      Object[] bindValues,
      Logger log
  );
}
