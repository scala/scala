/*    __  ______________                                                      *\
**   /  |/ / ____/ ____/                                                      **
**  / | | /___  / /___                                                        **
** /_/|__/_____/_____/ Copyright 2005-2007 LAMP/EPFL                          **
**                                                                            **
\*                                                                            */

// $Id$

package scala.tools.ant

/** <p>
 *    An Ant task to compile with the fast Scala compiler (<code>fsc</code>).
 *  </p>
 *  <p>
 *    In addition to the attributes shared with the <code>Scalac</code>
 *    task, this task also accepts the following attributes:
 *  </p>
 *  <ul style="font-family:Courier;">
 *    <li>reset</li>
 *    <li>server</li>
 *    <li>shutdown</li>
 *  </ul>
 *
 *  @author Stephane Micheloud
 */
class FastScalac extends Scalac {

  private var resetCaches: Boolean = false

  private var serverAddr: Option[String] = None

  private var shutdownServer: Boolean = false

/*============================================================================*\
**                             Properties setters                             **
\*============================================================================*/

  /** Sets the <code>reset</code> attribute. Used by Ant.
   *
   *  @param input The value for <code>reset</code>.
   */
  def setReset(input: Boolean): Unit =
    resetCaches = input

  /** Sets the <code>server</code> attribute. Used by Ant.
   *
   *  @param input The value for <code>server</code>.
   */
  def setServer(input: String): Unit = {
    def isHostNameValid(host: String): Boolean =
      try { val _ = java.net.InetAddress.getByName(host); true }
      catch { case _ => false }
    if (isHostNameValid(input)) serverAddr = Some(input)
    else error("Unknown server '" + input + "'")
  }

  /** Sets the <code>shutdown</code> attribute. Used by Ant.
   *
   *  @param input The value for <code>shutdown</code>.
   */
  def setShutdown(input: Boolean): Unit =
    shutdownServer = input

/*============================================================================*\
**                             The execute method                             **
\*============================================================================*/

  /** Performs the compilation. */
  override def execute() = {
    val Pair(settings, sourceFiles) = initialize

    if (!sourceFiles.isEmpty) {
      def trim(xs: List[String]) = xs filter (x => x.length > 0)
      val reset = settings.BooleanSetting("-reset", "Reset compile server caches")
      val shutdown = settings.BooleanSetting("-shutdown", "Shutdown compile server")

      reset.value = resetCaches
      shutdown.value = shutdownServer
      val cmdOptions =
        // StringSetting
        List.flatten(
          List(settings.outdir, settings.classpath, settings.bootclasspath,
               settings.extdirs, settings.encoding) map (s => List(s.nme, s.value))) :::
        // '-server' option
        (if (serverAddr.isEmpty) Nil else List("-server", serverAddr.get)) :::
        // ChoiceSetting
        (List(settings.debuginfo, settings.target) map (s => s.nme + ":" + s.value)) :::
        // BooleanSetting
        trim(
          List(settings.debug, settings.deprecation, settings.nopredefs,
               settings.verbose, reset, shutdown) map (s => if (s.value) s.nme else "")) :::
        // PhaseSetting
        trim(
          List(settings.log) map (s => if (s.value.isEmpty) "" else s.nme + ":" + s.value))

      val args = (cmdOptions ::: (sourceFiles map (.toString()))).toArray
      try {
        nsc.CompileClient.main0(args)
      } catch {
        case exception: Throwable if (exception.getMessage ne null) =>
          exception.printStackTrace()
          error("Compile failed because of an internal compiler error (" +
            exception.getMessage + "); see the error output for details.")
        case exception =>
          exception.printStackTrace()
          error("Compile failed because of an internal compiler error " +
            "(no error message provided); see the error output for details.")
      }
    }
  }
}
