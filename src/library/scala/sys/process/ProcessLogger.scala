/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys
package process

import java.io._

/** Encapsulates the output and error streams of a running process.
 *  Many of the methods of ProcessBuilder accept a ProcessLogger as
 *  an argument.
 *
 *  @see      ProcessBuilder
 */
trait ProcessLogger {
  /** Will be called with each line read from the process output stream.
   */
  def out(s: => String): Unit

  /** Will be called with each line read from the process error stream.
   */
  def err(s: => String): Unit

  /** If a process is begun with one of these ProcessBuilder methods:
   *
   *    def !(log: ProcessLogger): Int
   *    def !<(log: ProcessLogger): Int
   *
   *  The run will be wrapped in a call to buffer.  This gives the logger
   *  an opportunity to set up and tear down buffering.  At present the
   *  library implementations of ProcessLogger simply execute the body unbuffered.
   */
  def buffer[T](f: => T): T
}

class FileProcessLogger(file: File) extends ProcessLogger with Closeable with Flushable {
  private val writer = (
    new PrintWriter(
      new BufferedWriter(
        new OutputStreamWriter(
          new FileOutputStream(file, true)
        )
      )
    )
  )
  def out(s: => String): Unit = writer println s
  def err(s: => String): Unit = writer println s
  def buffer[T](f: => T): T = f
  def close(): Unit = writer.close()
  def flush(): Unit = writer.flush()
}

object ProcessLogger {
  def apply(file: File): FileProcessLogger = new FileProcessLogger(file)
  def apply(fn: String => Unit): ProcessLogger = apply(fn, fn)
  def apply(fout: String => Unit, ferr: String => Unit): ProcessLogger = new ProcessLogger {
    def out(s: => String): Unit = fout(s)
    def err(s: => String): Unit = ferr(s)
    def buffer[T](f: => T): T = f
  }
}
