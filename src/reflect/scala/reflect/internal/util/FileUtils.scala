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

package scala.reflect.internal.util

import java.io.{BufferedWriter, IOException, OutputStreamWriter, Writer}
import java.nio.CharBuffer
import java.nio.charset.{Charset, CharsetEncoder, StandardCharsets}, StandardCharsets.UTF_8
import java.nio.file.{Files, OpenOption, Path}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicBoolean


import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}
import scala.util.{Failure, Success}

object FileUtils {
  def newAsyncBufferedWriter(path: Path, charset: Charset = UTF_8, options: Array[OpenOption] = NO_OPTIONS, threadsafe: Boolean = false): LineWriter = {
    val encoder: CharsetEncoder = charset.newEncoder
    val writer = new OutputStreamWriter(Files.newOutputStream(path, options: _*), encoder)
    newAsyncBufferedWriter(new BufferedWriter(writer), threadsafe)
  }
  def newAsyncBufferedWriter(underlying: Writer, threadsafe: Boolean): LineWriter = {
    val async = new AsyncBufferedWriter(underlying)
    if (threadsafe) new ThreadsafeWriter(async) else async
  }
  private val NO_OPTIONS = new Array[OpenOption](0)

  sealed abstract class LineWriter extends Writer {
    def newLine(): Unit
  }
  private class ThreadsafeWriter(val underlying: AsyncBufferedWriter) extends LineWriter {
    lock = underlying
    override def write(c: Int): Unit =
      lock.synchronized (underlying.write(c))

    override def write(cbuf: Array[Char]): Unit =
      lock.synchronized (underlying.write(cbuf))

    override def write(cbuf: Array[Char], off: Int, len: Int): Unit =
      lock.synchronized (underlying.write(cbuf, off, len))

    override def write(str: String): Unit =
      lock.synchronized (underlying.write(str))

    override def write(str: String, off: Int, len: Int): Unit =
      lock.synchronized (underlying.write(str, off, len))

    override def flush(): Unit =
      lock.synchronized (underlying.flush())

    override def close(): Unit =
      lock.synchronized (underlying.close())

    override def newLine(): Unit =
      lock.synchronized (underlying.newLine())

  }

  private object AsyncBufferedWriter {
    private val Close = CharBuffer.allocate(0)
    private val Flush = CharBuffer.allocate(0)
  }
  private class AsyncBufferedWriter(val underlying: Writer, bufferSize : Int = 4096) extends LineWriter {
    private var current: CharBuffer = allocate
    override def write(c: Int): Unit = super.write(c)
    private def flushAsync(): Unit = {
      background.ensureProcessed(current)
      current = allocate
    }
//    allocate or reuse a CharArray which is guaranteed to have a backing array
    private def allocate: CharBuffer = {
      val reused = background.reuseBuffer
      if (reused eq null)      CharBuffer.allocate(bufferSize)
      else {
        //we don't care about race conditions
        background.reuseBuffer = null
        reused.clear()
        reused
      }
    }

    override def write(cbuf: Array[Char], initialOffset: Int, initialLength: Int): Unit = {
      var offset = initialOffset
      var length = initialLength
      while (length > 0) {
        val capacity = current.remaining()
        if (length <= capacity) {
          current.put(cbuf, offset, length)
          length = 0
        } else {
          current.put(cbuf, offset, capacity)
          flushAsync()
          length -= capacity
          offset += capacity
        }
      }
    }

    override def write(s: String,  initialOffset: Int, initialLength: Int): Unit = {
      var offset = initialOffset
      var length = initialLength
      while (length > 0) {
        val capacity = current.remaining()
        if (length <= capacity) {
          current.put(s, offset, offset + length)
          length = 0
        } else {
          current.put(s, offset, offset + capacity)
          flushAsync()
          length -= capacity
          offset += capacity
        }
      }
    }

    def newLine(): Unit = write(scala.util.Properties.lineSeparator)

    /** slightly breaks the flush contract in that the flush is not complete when the method returns */
    override def flush(): Unit = {
      flushAsync()
    }

    override def close(): Unit = {
      background.ensureProcessed(current)
      background.ensureProcessed(AsyncBufferedWriter.Close)
      current = null
      Await.result(background.asyncStatus.future, Duration.Inf)
      underlying.close()
    }
    private object background extends Runnable{

      import scala.concurrent.ExecutionContext.Implicits.global

      private val pending = new LinkedBlockingQueue[CharBuffer]
      //a failure detected will case an Failure, Success indicates a close
      val asyncStatus = Promise[Unit]()
      private val scheduled = new AtomicBoolean
      @volatile var reuseBuffer: CharBuffer = _

      def ensureProcessed(buffer: CharBuffer): Unit = {
        if (asyncStatus.isCompleted) {
          asyncStatus.future.value.get match {
            case Success(()) => throw new IllegalStateException("closed")
            case Failure(t) => throw new IOException("async failure", t)
          }
        }

        //order is essential - add to the queue before the CAS
        pending.add(buffer)
        if (scheduled.compareAndSet(false, true)) {
          global.execute(background)
        }
      }

      def run(): Unit = {
        try {
          while (!pending.isEmpty) {
            val next = pending.poll()
            if (next eq AsyncBufferedWriter.Flush) {
              underlying.flush()
            } else if (next eq AsyncBufferedWriter.Close) {
              underlying.flush()
              underlying.close()
              asyncStatus.trySuccess(())
            } else {
              val array = next.array()
              next.flip()
              underlying.write(array, next.arrayOffset() + next.position(), next.limit())
              reuseBuffer = next
            }
          }
        } catch {
          case t: Throwable =>
            asyncStatus.tryFailure(t)
            throw t
        }
        finally scheduled.set(false)

        //we are not scheduled any more
        //as a last check ensure that we didn't race with an addition to the queue
        //order is essential - queue is checked before CAS
        if ((!pending.isEmpty) && scheduled.compareAndSet(false, true)) {
          global.execute(background)
        }
      }
    }
  }
}
