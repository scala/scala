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

package scala.tools
package nsc
package interactive

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends nsc.MainClass {
  override def processSettingsHook(): Boolean = {
    def run(): Unit = {
      this.settings.Xprintpos.value = true
      this.settings.Yrangepos.value = true
      val compiler = new interactive.Global(this.settings, this.reporter)
      import compiler.{ reporter => _, _ }

      val sfs = command.files map getSourceFile
      val reloaded = new interactive.Response[Unit]
      askReload(sfs, reloaded)

      reloaded.get.right.toOption match {
        case Some(ex) => reporter.cancelled = true // Causes exit code to be non-0
        case None => reporter.reset() // Causes other compiler errors to be ignored
      }
      askShutdown
    }
    super.processSettingsHook() && (
      if (this.settings.Yidedebug) { run() ; false } else true
    )
  }
}
