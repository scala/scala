/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import typechecker.Analyzer

/** A layer on top of Global so I can guarantee some extra
 *  functionality for the repl.  It doesn't do much yet.
 */
trait ReplGlobal extends Global {
  // This exists mostly because using the reporter too early leads to deadlock.
  private def echo(msg: String) { Console println msg }

  override def abort(msg: String): Nothing = {
    echo("ReplGlobal.abort: " + msg)
    super.abort(msg)
  }

  override lazy val analyzer = new {
    val global: ReplGlobal.this.type = ReplGlobal.this
  } with Analyzer {

    override protected def findMacroClassLoader(): ClassLoader = {
      val loader = super.findMacroClassLoader
      macroLogVerbose("macro classloader: initializing from a REPL classloader: %s".format(global.classPath.asURLs))
      val virtualDirectory = globalSettings.outputDirs.getSingleOutput.get
      new util.AbstractFileClassLoader(virtualDirectory, loader) {}
    }
  }
}
