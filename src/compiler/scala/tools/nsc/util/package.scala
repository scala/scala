/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package tools
package nsc

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream, PrintWriter, StringWriter, Reader }
import scala.collection.immutable.ArraySeq

package object util {
  // forwarder for old code that builds against 2.9 and 2.10
  val Chars = scala.reflect.internal.Chars

  type Set[T <: AnyRef] = scala.reflect.internal.util.Set[T]
  type HashSet[T >: Null <: AnyRef] = scala.reflect.internal.util.HashSet[T]
  val HashSet = scala.reflect.internal.util.HashSet

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
    def allThreads(): IndexedSeq[Thread] = {
      val tarray = new Array[Thread](Thread.activeCount())
      val got    = Thread.enumerate(tarray)

      ArraySeq.unsafeWrapArray(tarray.take(got))
    }

    val ts1    = allThreads()
    val result = body
    val ts2    = allThreads()

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
  def stackTraceString(ex: Throwable): String = stringFromWriter(ex.printStackTrace)

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
     *  The format is similar to the typical case described in the Javadoc
     *  for [[java.lang.Throwable#printStackTrace()*]].
     *  If a stack trace is truncated, it will be followed by a line of the form
     *  `... 3 elided`, by analogy to the lines `... 3 more` which indicate
     *  shared stack trace segments.
     *  @param p the predicate to select the prefix
     */
    def stackTracePrefixString(p: StackTraceElement => Boolean): String = stackTracePrefixString(e)(p)
  }

  implicit class `quickie stack dump`(private val sc: StringContext) extends AnyVal {
    @deprecated("For debug only", since="forever")
    def trace(args: Any*): Unit = new Throwable(sc.s(args: _*)).printStackTrace()
  }

  lazy val trace = new SimpleTracer(System.out)

  // These four deprecated since 2.10.0 are still used in
  // the sbt 0.13 compiler interface.
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
