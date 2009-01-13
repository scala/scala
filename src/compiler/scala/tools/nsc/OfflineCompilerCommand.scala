/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc

/** A compiler command for the offline compiler.
 *
 * @author Martin Odersky and Lex Spoon
 */
class OfflineCompilerCommand(
  arguments: List[String],
  settings: Settings,
  error: String => Unit,
  interactive: Boolean)
extends CompilerCommand(arguments, new Settings(error), error, false)
{
  override val cmdName = "fsc"
  settings.disable(settings.prompt)
  settings.disable(settings.resident)
  new settings.BooleanSetting("-reset", "Reset compile server caches")
  new settings.BooleanSetting("-shutdown", "Shutdown compile server")
  new settings.StringSetting("-server", "hostname:portnumber",
                             "Specify compile server socket", "")
  new settings.BooleanSetting("-J<flag>", "Pass <flag> directly to runtime system")
}
