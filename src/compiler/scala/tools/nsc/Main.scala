/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package nsc

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
class MainClass extends Driver with EvalLoop {
  def resident(compiler: Global): Unit = loop { line =>
    val command = new CompilerCommand(line.split("\\s+").toList, new Settings(scalacError))
    compiler.reporter.reset()
    new compiler.Run() compile command.files
  }

  override def newCompiler(): Global = Global(settings)

  override def doCompile(compiler: Global): Unit = {
    if (settings.resident.value) resident(compiler)
    else super.doCompile(compiler)
  }
}

object Main extends MainClass { }
