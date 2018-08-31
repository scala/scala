/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
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

      val sfs = command.files map compiler.getSourceFile
      val reloaded = new interactive.Response[Unit]
      compiler.askReload(sfs, reloaded)

      reloaded.get.toOption match {
        case Some(ex) => reporter.ERROR.count += 1 // Causes exit code to be non-0
        case None     => reporter.reset()          // Causes other compiler errors to be ignored
      }
      compiler.askShutdown()
    }
    super.processSettingsHook() && (
      if (this.settings.Yidedebug) { run() ; false } else true
    )
  }
}
