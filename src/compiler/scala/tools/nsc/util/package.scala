/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Paul Phillips
 */

package scala
package tools
package nsc

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream, PrintWriter, StringWriter, Reader }

package object util {
  // forwarder for old code that builds against 2.9 and 2.10
  val Chars = scala.reflect.internal.Chars

  type Set[T <: AnyRef] = scala.reflect.internal.util.Set[T]
  type HashSet[T >: Null <: AnyRef] = scala.reflect.internal.util.HashSet[T]
  val HashSet = scala.reflect.internal.util.HashSet

  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  /** Execute code and then wait for all non-daemon Threads
   *  created and begun during its execution to complete.
   */
  def waitingForThreads[T](body: => T) = {
    val (result, created) = trackingThreads(body)
    val threads = created filterNot (_.isDaemon)

    // As long as there are non-daemon, live threads (the latter
    // condition should exclude shutdown hooks) we will wait.
    while (threads exists (_.isAlive))
      threads filter (_.isAlive) foreach (_.join())

    result
  }

  /** Executes the code and returns the result and any threads
   *  which were created during its execution.
   */
  def trackingThreads[T](body: => T): (T, Seq[Thread]) = {
    val ts1    = sys.allThreads()
    val result = body
    val ts2    = sys.allThreads()

    (result, ts2 filterNot (ts1 contains _))
  }

  def stringFromReader(reader: Reader) = {
    val writer = new StringWriter()
    var c = reader.read()
    while(c != -1) {
      writer.write(c)
      c = reader.read()
    }
    reader.close()
    writer.toString()
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

  /** A one line string which contains the class of the exception, the
   *  message if any, and the first non-Predef location in the stack trace
   *  (to exclude assert, require, etc.)
   */
  def stackTraceHeadString(ex: Throwable): String = {
    val frame = ex.getStackTrace.dropWhile(_.getClassName contains "Predef") take 1 mkString ""
    val msg   = ex.getMessage match { case null | "" => "" ; case s => s"""("$s")""" }
    val clazz = ex.getClass.getName.split('.').last

    s"$clazz$msg @ $frame"
  }

  implicit class StackTraceOps(private val e: Throwable) extends AnyVal with StackTracing {
    /** Format the stack trace, returning the prefix consisting of frames that satisfy
     *  a given predicate.
     *  The format is similar to the typical case described in the JavaDoc
     *  for [[java.lang.Throwable#printStackTrace]].
     *  If a stack trace is truncated, it will be followed by a line of the form
     *  `... 3 elided`, by analogy to the lines `... 3 more` which indicate
     *  shared stack trace segments.
     *  @param p the predicate to select the prefix
     */
    def stackTracePrefixString(p: StackTraceElement => Boolean): String = stackTracePrefixString(e)(p)
  }

  lazy val trace = new SimpleTracer(System.out)

  // These four deprecated since 2.10.0 are still used in (at least)
  // the sbt 0.12.4 compiler interface.
  @deprecated("Moved to scala.reflect.internal.util.Position", "2.10.0")
  type Position = scala.reflect.internal.util.Position
  @deprecated("Moved to scala.reflect.internal.util.NoPosition", "2.10.0")
  val NoPosition = scala.reflect.internal.util.NoPosition
  @deprecated("Moved to scala.reflect.internal.util.FakePos", "2.10.0")
  val FakePos = scala.reflect.internal.util.FakePos
  @deprecated("Moved to scala.reflect.internal.util.FakePos", "2.10.0")
  type FakePos = scala.reflect.internal.util.FakePos

  // These three were still used in scala-refactoring.
  @deprecated("Moved to scala.reflect.internal.util.RangePosition", "2.10.0")
  type RangePosition = scala.reflect.internal.util.RangePosition
  @deprecated("Moved to scala.reflect.internal.util.SourceFile", "2.10.0")
  type SourceFile = scala.reflect.internal.util.SourceFile
  @deprecated("Moved to scala.reflect.internal.util.BatchSourceFile", "2.10.0")
  type BatchSourceFile = scala.reflect.internal.util.BatchSourceFile

  @deprecated("Moved to scala.reflect.internal.util.AbstractFileClassLoader", "2.11.0")
  type AbstractFileClassLoader = scala.reflect.internal.util.AbstractFileClassLoader

  @deprecated("Moved to scala.reflect.internal.util.ScalaClassLoader", "2.11.0")
  val ScalaClassLoader = scala.reflect.internal.util.ScalaClassLoader

  @deprecated("Moved to scala.reflect.internal.util.ScalaClassLoader", "2.11.0")
  type ScalaClassLoader = scala.reflect.internal.util.ScalaClassLoader
}
