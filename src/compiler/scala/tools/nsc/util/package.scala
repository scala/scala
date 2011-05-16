/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream, PrintWriter, StringWriter }

package object util {

  type Set[T <: AnyRef] = scala.reflect.common.util.Set[T]
  type HashSet[T >: Null <: AnyRef] = scala.reflect.common.util.HashSet[T]
  val HashSet = scala.reflect.common.util.HashSet

  def onull[T](value: T, orElse: => T): T = if (value == null) orElse else value

  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  /** Frequency counter */
  def freq[T](xs: Traversable[T]): Map[T, Int] = xs groupBy identity mapValues (_.size)

  def freqrank[T](xs: Traversable[(T, Int)]): List[(Int, T)] = xs.toList map (_.swap) sortBy (-_._1)

  /** Execute code and then wait for all Threads created during its
   *  execution to complete.
   */
  def waitingForThreads[T](body: => T) = {
    val ts1        = sys.allThreads()
    val result     = body
    val ts2        = sys.allThreads()
    val newThreads = ts2.toSet -- ts1 filterNot (_.isDaemon())

    newThreads foreach (_.join())
    result
  }

  /** Given a function and a block of code, evaluates code block,
   *  calls function with milliseconds elapsed, and returns block result.
   */
  def millisElapsedTo[T](f: Long => Unit)(body: => T): T = {
    val start = System.currentTimeMillis
    val result = body
    val end = System.currentTimeMillis

    f(end - start)
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
  def stackTraceString(ex: Throwable): String = stringFromWriter(ex printStackTrace _)
}
