/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream, PrintWriter, StringWriter }

package object util {
  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  /** Register a shutdown hook to be run when the jvm exits.
   *  Marks it as daemon so it doesn't interfere with shutdown,
   *  but the thread is returned so it can be modified.
   */
  def addShutdownHook(body: => Unit) = {
    returning(new Thread { override def run { body } }) { t =>
      t setDaemon true
      Runtime.getRuntime addShutdownHook t
    }
  }

  /** All living threads. */
  def allThreads(): List[Thread] = {
    val num = Thread.activeCount()
    val tarray = new Array[Thread](num)
    val got = Thread.enumerate(tarray)

    tarray take got toList
  }

  /** Execute code and then wait for all Threads created during its
   *  execution to complete.
   */
  def waitingForThreads[T](body: => T) = {
    val ts1 = allThreads()
    val result = body
    val ts2 = allThreads()
    val newThreads = (ts2.toSet -- ts1) filterNot (_.isDaemon())

    newThreads foreach (_.join())
    result
  }

  /** Generate a string using a routine that wants to write on a stream. */
  def stringFromWriter(writer: PrintWriter => Unit): String = {
    val stringWriter = new StringWriter()
    val stream = new NewLinePrintWriter(stringWriter)
    writer(stream)
    stream.close()
    stringWriter.toString
  }
  def stringFromStream(stream: OutputStream => Unit): String = {
    val bs = new ByteArrayOutputStream()
    val ps = new PrintStream(bs)
    stream(ps)
    ps.close()
    bs.toString()
  }
}
