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
