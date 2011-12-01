/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// Developer note:
//   scala -J-Dscala.process.debug
// for process debugging output.
//
package scala.sys {
  /**
    * This package is used to create process pipelines, similar to Unix command pipelines.
    *
    * The key concept is that one builds a [[scala.sys.process.Process]] that will run and return an exit
    * value.  This `Process` is usually composed of one or more [[scala.sys.process.ProcessBuilder]], fed by a
    * [[scala.sys.process.ProcessBuilder.Source]] and feeding a [[scala.sys.process.ProcessBuilder.Sink]]. A
    * `ProcessBuilder` itself is both a `Source` and a `Sink`.
    *
    * As `ProcessBuilder`, `Sink` and `Source` are abstract, one usually creates them with `apply` methods on
    * the companion object of [[scala.sys.process.Process]], or through implicit conversions available in this
    * package object from `String` and other types. The pipe is composed through unix-like pipeline and I/O
    * redirection operators available on [[scala.sys.process.ProcessBuilder]].
    *
    * The example below shows how to build and combine such commands. It searches for `null` uses in the `src`
    * directory, printing a message indicating whether they were found or not. The first command pipes its
    * output to the second command, whose exit value is then used to choose between the third or fourth
    * commands. This same example is explained in greater detail on [[scala.sys.process.ProcessBuilder]].
    *
    * {{{
    * import scala.sys.process._
    * (
    *     "find src -name *.scala -exec grep null {} ;"
    *     #|  "xargs test -z"
    *     #&&  "echo null-free"  #||  "echo null detected"
    * ) !
    * }}}
    *
    * Other implicits available here are for [[scala.sys.process.ProcessBuilder.FileBuilder]], which extends
    * both `Sink` and `Source`, and for [[scala.sys.process.ProcessBuilder.URLBuilder]], which extends
    * `Source` alone.
    *
    * One can even create a `Process` solely out of these, without running any command. For example, this will
    * download from a URL to a file:
    *
    * {{{
    * import java.io.File
    * import java.net.URL
    * import scala.sys.process._
    * new URL("http://www.scala-lang.org/") #> new File("scala-lang.html") !
    * }}}
    *
    * One may use a `Process` directly through `ProcessBuilder`'s `run` method, which starts the process in
    * the background, and returns a `Process`. If background execution is not desired, one can get a
    * `ProcessBuilder` to execute through a method such as `!`, `lines`, `run` or variations thereof. That
    * will create the `Process` to execute the commands, and return either the exit value or the output, maybe
    * throwing an exception.
    *
    * Finally, when executing a `ProcessBuilder`, one may pass a [[scala.sys.process.ProcessLogger]] to
    * capture stdout and stderr of the executing processes. A `ProcessLogger` may be created through its
    * companion object from functions of type `(String) => Unit`, or one might redirect it to a file, using
    * [[scala.sys.process.FileProcessLogger]], which can also be created through `ProcessLogger`'s object
    * companion.
    */
  package object process extends ProcessImplicits {
    def javaVmArguments: List[String] = {
      import collection.JavaConversions._

      java.lang.management.ManagementFactory.getRuntimeMXBean().getInputArguments().toList
    }
    def stdin  = java.lang.System.in
    def stdout = java.lang.System.out
    def stderr = java.lang.System.err
  }
  // private val shell: String => Array[String] =
  //   if (isWin) Array("cmd.exe", "/C", _)
  //   else Array("sh", "-c", _)

  package process {
    // These are in a nested object instead of at the package level
    // due to the issues described in tickets #3160 and #3836.
    private[process] object processInternal {
      final val processDebug = props contains "scala.process.debug"
      dbg("Initializing process package.")

      type =?>[-A, +B]     = PartialFunction[A, B]
      type Closeable       = java.io.Closeable
      type File            = java.io.File
      type IOException     = java.io.IOException
      type InputStream     = java.io.InputStream
      type JProcess        = java.lang.Process
      type JProcessBuilder = java.lang.ProcessBuilder
      type OutputStream    = java.io.OutputStream
      type SyncVar[T]      = scala.concurrent.SyncVar[T]
      type URL             = java.net.URL

      def onInterrupt[T](handler: => T): Throwable =?> T = {
        case _: InterruptedException => handler
      }

      def ioFailure[T](handler: IOException => T): Throwable =?> T = {
        case e: IOException => handler(e)
      }

      def dbg(msgs: Any*) = if (processDebug) {
        Console.println("[process] " + (msgs mkString " "))
      }
    }
  }
}
