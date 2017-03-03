/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.tools
package nsc

import scala.language.postfixOps

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
class MainClass extends Driver with EvalLoop {
  def resident(compiler: Global): Unit = loop { line =>
    val command = new CompilerCommand(line split "\\s+" toList, new Settings(scalacError))
    compiler.reporter.reset()
    new compiler.Run() compile command.files
  }

  override def newCompiler(): Global = Global(settings)

  override def doCompile(compiler: Global) {
    if (settings.resident) resident(compiler)
    else super.doCompile(compiler)
  }
}

object Main extends MainClass { }
