/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Paul Phillips
 */

package scala.tools.nsc

import java.io.{ OutputStream, PrintStream, ByteArrayOutputStream, PrintWriter, StringWriter }

package object util {

  implicit def postfixOps = language.postfixOps // make all postfix ops in this package compile without warning

  // forwarder for old code that builds against 2.9 and 2.10
  val Chars = scala.reflect.internal.Chars

  type Set[T <: AnyRef] = scala.reflect.internal.util.Set[T]
  type HashSet[T >: Null <: AnyRef] = scala.reflect.internal.util.HashSet[T]
  val HashSet = scala.reflect.internal.util.HashSet

  def onull[T](value: T, orElse: => T): T = if (value == null) orElse else value

  /** Apply a function and return the passed value */
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }

  /** Frequency counter */
  def freq[T](xs: Traversable[T]): Map[T, Int] = xs groupBy identity mapValues (_.size)

  def freqrank[T](xs: Traversable[(T, Int)]): List[(Int, T)] = xs.toList map (_.swap) sortBy (-_._1)

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

  lazy val trace = new SimpleTracer(System.out)
  lazy val errtrace = new SimpleTracer(System.err)

  @deprecated("Moved to scala.reflect.internal.util.StringOps", "2.10.0")
  val StringOps = scala.reflect.internal.util.StringOps

  @deprecated("Moved to scala.reflect.internal.util.StringOps", "2.10.0")
  type StringOps = scala.reflect.internal.util.StringOps

  @deprecated("Moved to scala.reflect.internal.util.TableDef", "2.10.0")
  val TableDef = scala.reflect.internal.util.TableDef

  @deprecated("Moved to scala.reflect.internal.util.TableDef", "2.10.0")
  type TableDef[T] = scala.reflect.internal.util.TableDef[T]

  @deprecated("scala.reflect.internal.util.WeakHashSet", "2.10.0")
  type WeakHashSet[T <: AnyRef] = scala.reflect.internal.util.WeakHashSet[T]

  @deprecated("Moved to scala.reflect.internal.util.Position", "2.10.0")
  val Position = scala.reflect.internal.util.Position

  @deprecated("Moved to scala.reflect.internal.util.Position", "2.10.0")
  type Position = scala.reflect.internal.util.Position

  @deprecated("Moved to scala.reflect.internal.util.NoPosition", "2.10.0")
  val NoPosition = scala.reflect.internal.util.NoPosition

  @deprecated("Moved to scala.reflect.internal.util.FakePos", "2.10.0")
  val FakePos = scala.reflect.internal.util.FakePos

  @deprecated("Moved to scala.reflect.internal.util.FakePos", "2.10.0")
  type FakePos = scala.reflect.internal.util.FakePos

  @deprecated("Moved to scala.reflect.internal.util.OffsetPosition", "2.10.0")
  type OffsetPosition = scala.reflect.internal.util.OffsetPosition

  @deprecated("Moved to scala.reflect.internal.util.RangePosition", "2.10.0")
  type RangePosition = scala.reflect.internal.util.RangePosition

  @deprecated("Moved to scala.reflect.internal.util.SourceFile", "2.10.0")
  type SourceFile = scala.reflect.internal.util.SourceFile

  @deprecated("Moved to scala.reflect.internal.util.NoSourceFile", "2.10.0")
  val NoSourceFile = scala.reflect.internal.util.NoSourceFile

  @deprecated("Moved to scala.reflect.internal.util.NoFile", "2.10.0")
  val NoFile = scala.reflect.internal.util.NoFile

  @deprecated("Moved to scala.reflect.internal.util.ScriptSourceFile", "2.10.0")
  val ScriptSourceFile = scala.reflect.internal.util.ScriptSourceFile

  @deprecated("Moved to scala.reflect.internal.util.ScriptSourceFile", "2.10.0")
  type ScriptSourceFile = scala.reflect.internal.util.ScriptSourceFile

  @deprecated("Moved to scala.reflect.internal.util.BatchSourceFile", "2.10.0")
  type BatchSourceFile = scala.reflect.internal.util.BatchSourceFile
}
