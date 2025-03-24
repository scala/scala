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
package interactive

import scala.reflect.internal.util.NoPosition

/** The main class for NSC, a compiler for the programming
 *  language Scala.
 */
object Main extends nsc.MainClass {
  override def processSettingsHook(): Boolean = {
    def run(): Unit = {
      this.settings.Xprintpos.value = true
      this.settings.Yrangepos.value = true
      val compiler = new interactive.Global(this.settings, this.reporter)

      val sfs = command.files map compiler.getSourceFile
      val reloaded = new interactive.Response[Unit]
      compiler.askReload(sfs, reloaded)

      reloaded.get.toOption match {
        case Some(ex) => reporter.error(NoPosition, ex.getMessage) // Causes exit code to be non-0
        case None     => reporter.reset()                          // Causes other compiler errors to be ignored
      }
      compiler.askShutdown()
    }
    super.processSettingsHook() && (
      if (this.settings.Yidedebug.value) { run() ; false } else true
    )
  }
}
