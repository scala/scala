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

package scala.tools.nsc
package interpreter

import scala.reflect.internal.util.AbstractFileClassLoader
import scala.tools.nsc.classpath.{AggregateClassPath, ClassPathFactory}
import scala.tools.nsc.util.ClassPath

trait ReplGlobal extends Global {

  override def abort(msg: String): Nothing = {
    // Using the reporter too early leads to deadlock. TODO: is this still true?
    Console.println("ReplGlobal.abort: " + msg)
    super.abort(msg)
  }

  override def findMacroClassLoader(): ClassLoader = {
    val loader = super.findMacroClassLoader
    analyzer.macroLogVerbose("macro classloader: initializing from a REPL classloader: %s".format(classPath.asURLs))
    val virtualDirectory = analyzer.globalSettings.outputDirs.getSingleOutput.get
    new AbstractFileClassLoader(virtualDirectory, loader) {}
  }

  override def optimizerClassPath(base: ClassPath): ClassPath = {
    settings.outputDirs.getSingleOutput match {
      case None => base
      case Some(out) =>
        // Make bytecode of previous lines available to the inliner
        val replOutClasspath = ClassPathFactory.newClassPath(settings.outputDirs.getSingleOutput.get, settings, closeableRegistry)
        AggregateClassPath.createAggregate(platform.classPath, replOutClasspath)
    }
  }

  override def toString = "<global>"
}
