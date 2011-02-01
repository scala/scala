/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

/** A compiler command for the offline compiler.
 *
 * @author Martin Odersky and Lex Spoon
 */
class OfflineCompilerCommand(arguments: List[String], _settings: Settings)
                  extends CompilerCommand(arguments, _settings)
{
  override def cmdName = "fsc"
  import settings._

  disable(prompt)
  disable(resident)

  def verbose = settings.verbose.value
  def debug = settings.debug.value

  val fscReset    = BooleanSetting("-reset",    "Reset compile server caches")
  val fscShutdown = BooleanSetting("-shutdown", "Shutdown compile server")
  val fscServer   = StringSetting ("-server",   "hostname:portnumber", "Specify compile server socket", "")
}
