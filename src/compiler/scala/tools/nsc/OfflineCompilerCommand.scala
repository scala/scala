/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

/** A compiler command for the offline compiler.
 *
 * @author Martin Odersky and Lex Spoon
 */
class OfflineCompilerCommand(
  arguments: List[String],
  settings: Settings)
extends CompilerCommand(arguments, settings) {
  override val cmdName = "fsc"
  import settings._

  disable(prompt)
  disable(resident)

  BooleanSetting("-reset",    "Reset compile server caches")
  BooleanSetting("-shutdown", "Shutdown compile server")
  StringSetting ("-server",   "hostname:portnumber", "Specify compile server socket", "")
  BooleanSetting("-J<flag>",  "Pass <flag> directly to runtime system")
}
