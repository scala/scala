/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools.ant

/** An Ant task to compile with the fast Scala compiler (`fsc`).
 *
 *  In addition to the attributes shared with the `Scalac` task, this task
 *  also accepts the following attributes:
 *  - `reset`
 *  - `server`
 *  - `shutdown`
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

  /** Sets the `reset` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `reset`.
   */
  def setReset(input: Boolean) { resetCaches = input }

  /** Sets the `server` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `server`.
   */
  def setServer(input: String) { serverAddr = Some(input) }

  /** Sets the `shutdown` attribute. Used by [[http://ant.apache.org Ant]].
   *
   *  @param input The value for `shutdown`.
   */
  def setShutdown(input: Boolean) { shutdownServer = input }

/*============================================================================*\
**                             The execute method                             **
\*============================================================================*/

  /** Performs the compilation. */
  override def execute() = {
    val (settings, sourceFiles, javaOnly) = initialize
    val s = settings

    if (!sourceFiles.isEmpty && !javaOnly) {
      def trim(xs: List[String]) = xs filter (x => x.length > 0)
      val reset = settings.BooleanSetting("-reset", "Reset compile server caches")
      val shutdown = settings.BooleanSetting("-shutdown", "Shutdown compile server")

      reset.value = resetCaches
      shutdown.value = shutdownServer

      /** XXX Since fsc is largely unmaintained, the set of options being
       *  individually assessed here is likely to bear little relationship to
       *  the current set of options. Most likely this manifests in confusing
       *  and very difficult to debug behavior in fsc. We should warn or fix.
       */
      val stringSettings =
        List(s.outdir, s.classpath, s.bootclasspath, s.extdirs, s.encoding) flatMap (x => List(x.name, x.value))

      val serverOption =
        serverAddr.toList flatMap (x => List("-server", x))  // '-server' option

      val choiceSettings =
        List(s.debuginfo, s.target) map (x => "%s:%s".format(x.name, x.value))

      val booleanSettings =
        List(s.debug, s.deprecation, s.verbose, reset, shutdown) map (x => if (x.value) List(x.name) else Nil) flatten

      val phaseSetting = {
        val s = settings.log
        if (s.value.isEmpty) Nil
        else List("%s:%s".format(s.name, s.value.mkString(",")))
      }

      val cmdOptions =
        stringSettings ::: serverOption ::: choiceSettings ::: booleanSettings ::: phaseSetting

      val args = (cmdOptions ::: (sourceFiles map (_.toString))).toArray
      try {
        if (scala.tools.nsc.CompileClient.process(args) && failonerror)
          buildError("Compile failed; see the compiler error output for details.")
      }
      catch {
        case exception: Throwable if exception.getMessage ne null =>
          exception.printStackTrace()
          buildError("Compile failed because of an internal compiler error (" +
            exception.getMessage + "); see the error output for details.")
        case exception =>
          exception.printStackTrace()
          buildError("Compile failed because of an internal compiler error " +
            "(no error message provided); see the error output for details.")
      }
    }
  }
}
