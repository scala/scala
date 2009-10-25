/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

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
    serverAddr = Some(input)
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
    val (settings, sourceFiles, javaOnly) = initialize
    val s = settings

    if (!sourceFiles.isEmpty && !javaOnly) {
      def trim(xs: List[String]) = xs filter (x => x.length > 0)
      val reset = settings.BooleanSetting("-reset", "Reset compile server caches")
      val shutdown = settings.BooleanSetting("-shutdown", "Shutdown compile server")

      reset.value = resetCaches
      shutdown.value = shutdownServer

      val stringSettings =
        List(s.outdir, s.classpath, s.bootclasspath, s.extdirs, s.encoding) flatMap (x => List(x.name, x.value))

      val serverOption =
        serverAddr.toList flatMap (x => List("-server", x))  // '-server' option

      val choiceSettings =
        List(s.debuginfo, s.target) map (x => "%s:%s".format(x.name, x.value))

      val booleanSettings =
        List(s.debug, s.deprecation, s.nopredefs, s.verbose, reset, shutdown) map (x => if (x.value) List(x.name) else Nil) flatten

      val phaseSetting = {
        val s = settings.log
        if (s.value.isEmpty) Nil
        else List("%s:%s".format(s.name, s.value.mkString(",")))
      }

      val cmdOptions =
        stringSettings ::: serverOption ::: choiceSettings ::: booleanSettings ::: phaseSetting

      val args = (cmdOptions ::: (sourceFiles map (_.toString))).toArray
      try {
        if (scala.tools.nsc.CompileClient.main0(args) > 0 && failonerror)
          error("Compile failed; see the compiler error output for details.")
      }
      catch {
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
