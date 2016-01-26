package scala.sys.process

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import java.io.{InputStream, OutputStream, PipedInputStream, PipedOutputStream, ByteArrayInputStream,
  ByteArrayOutputStream, IOException, Closeable}
import java.lang.reflect.InvocationTargetException
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.Exception.ignoring

// Each test normally ends in a moment, but for failure cases, waits until one second.

@RunWith(classOf[JUnit4])
class PipedProcessTest {
  class ProcessMock(error: Boolean) extends Process {
    var destroyCount = 0
    def isAlive() = false
    def exitValue(): Int = {
      if (error) {
        throw new InterruptedException()
      }
      0
    }
    def destroy(): Unit = { destroyCount += 1 }
  }

  class ProcessBuilderMock(process: Process, error: Boolean) extends ProcessBuilder.AbstractBuilder {
    override def run(io: ProcessIO): Process = {
      if (error) {
        throw new IOException()
      }
      process
    }
  }

  class PipeSinkMock extends Process.PipeSink("PipeSinkMock") {
    var releaseCount = 0
    override val pipe = null
    override val sink = null
    override def run(): Unit = {}
    override def connectOut(out: OutputStream): Unit = {}
    override def connectIn(pipeOut: PipedOutputStream): Unit = {}
    override def release(): Unit = { releaseCount += 1 }
  }

  class PipeSourceMock extends Process.PipeSource("PipeSourceMock") {
    var releaseCount = 0
    override val pipe = null
    override val source = null
    override def run(): Unit = {}
    override def connectIn(in: InputStream): Unit = {}
    override def connectOut(sink: Process.PipeSink): Unit = {}
    override def release(): Unit = { releaseCount += 1 }
  }

  class PipedProcesses(a: ProcessBuilder, b: ProcessBuilder, defaultIO: ProcessIO, toError: Boolean)
    extends Process.PipedProcesses(a, b, defaultIO, toError) {
    def callRunAndExitValue(source: Process.PipeSource, sink: Process.PipeSink) = {
      val m = classOf[Process.PipedProcesses].getDeclaredMethod("runAndExitValue", classOf[Process.PipeSource], classOf[Process.PipeSink])
      m.setAccessible(true)
      try m.invoke(this, source, sink).asInstanceOf[Option[Int]]
      catch {
        case err: InvocationTargetException => throw err.getTargetException
      }
    }
  }

  // PipedProcesses need not to release resources when it normally end
  @Test
  def normallyEnd() {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    Await.result(f, Duration(1, SECONDS))
    assert(source.releaseCount == 0)
    assert(sink.releaseCount == 0)
    assert(a.destroyCount == 0)
    assert(b.destroyCount == 0)
  }

  // PipedProcesses must release resources when b.run() failed
  @Test
  def bFailed() {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = true), io, false)
    val f = Future {
      ignoring(classOf[IOException]) {
        p.callRunAndExitValue(source, sink)
      }
    }
    Await.result(f, Duration(1, SECONDS))
    assert(source.releaseCount == 1)
    assert(sink.releaseCount == 1)
    assert(a.destroyCount == 0)
    assert(b.destroyCount == 0)
  }

  // PipedProcesses must release resources when a.run() failed
  @Test
  def aFailed() {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = true), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      ignoring(classOf[IOException]) {
        p.callRunAndExitValue(source, sink)
      }
    }
    Await.result(f, Duration(1, SECONDS))
    assert(source.releaseCount == 1)
    assert(sink.releaseCount == 1)
    assert(a.destroyCount == 0)
    assert(b.destroyCount == 1)
  }

  // PipedProcesses must release resources when interrupted during waiting for first.exitValue()
  @Test
  def firstInterrupted() {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = true)
    val b = new ProcessMock(error = false)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    Await.result(f, Duration(1, SECONDS))
    assert(source.releaseCount == 1)
    assert(sink.releaseCount == 1)
    assert(a.destroyCount == 1)
    assert(b.destroyCount == 1)
  }

  // PipedProcesses must release resources when interrupted during waiting for second.exitValue()
  @Test
  def secondInterrupted() {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = true)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    Await.result(f, Duration(1, SECONDS))
    assert(source.releaseCount == 1)
    assert(sink.releaseCount == 1)
    assert(a.destroyCount == 1)
    assert(b.destroyCount == 1)
  }
}

@RunWith(classOf[JUnit4])
class PipeSourceSinkTest {
  def throwsIOException(f: => Unit) = {
    try { f; false }
    catch { case _: IOException => true }
  }

  class PipeSink extends Process.PipeSink("TestPipeSink") {
    def ensureRunloopStarted() = {
      while (sink.size() > 0) {
        Thread.sleep(1)
      }
    }
    def isReleased = {
      val field = classOf[Process.PipeSink].getDeclaredField("pipe")
      field.setAccessible(true)
      val pipe = field.get(this).asInstanceOf[PipedInputStream]
      !this.isAlive && throwsIOException { pipe.read() }
    }
  }

  class PipeSource extends Process.PipeSource("TestPipeSource") {
    def ensureRunloopStarted() = {
      while (source.size() > 0) {
        Thread.sleep(1)
      }
    }
    def isReleased = {
      val field = classOf[Process.PipeSource].getDeclaredField("pipe")
      field.setAccessible(true)
      val pipe = field.get(this).asInstanceOf[PipedOutputStream]
      !this.isAlive && throwsIOException { pipe.write(1) }
    }
  }

  trait CloseChecking extends Closeable {
    var closed = false
    override def close() = closed = true
  }
  class DebugOutputStream extends ByteArrayOutputStream with CloseChecking
  class DebugInputStream(s: String) extends ByteArrayInputStream(s.getBytes()) with CloseChecking
  class DebugInfinityInputStream extends InputStream with CloseChecking {
    def read() = 1
  }

  def sourceSink() = {
    val source = new PipeSource
    val sink = new PipeSink
    source connectOut sink
    source.start()
    sink.start()
    (source, sink)
  }

  // PipeSource and PipeSink must release resources when it normally end
  @Test
  def normallyEnd() {
    val in = new DebugInputStream("aaa")
    val (source, sink) = sourceSink()
    val out = new DebugOutputStream
    source connectIn in
    sink connectOut out
    val f = Future {
      source.join()
      sink.join()
    }
    Await.result(f, Duration(1, SECONDS))
    assert(in.closed == true)
    assert(out.closed == true)
    assert(source.isReleased == true)
    assert(sink.isReleased == true)
  }

  // PipeSource and PipeSink must release resources when interrupted during waiting for source.take()
  @Test
  def sourceInterrupted() {
    val (source, sink) = sourceSink()
    val out = new DebugOutputStream
    sink connectOut out
    val f = Future {
      sink.ensureRunloopStarted()
      source.release()
      sink.release()
    }
    Await.result(f, Duration(1, SECONDS))
    assert(out.closed == true)
    assert(source.isReleased == true)
    assert(sink.isReleased == true)
  }

  // PipeSource and PipeSink must release resources when interrupted during waiting for sink.take()
  @Test
  def sinkInterrupted() {
    val in = new DebugInputStream("aaa")
    val (source, sink) = sourceSink()
    source connectIn in
    val f = Future {
      source.ensureRunloopStarted()
      source.release()
      sink.release()
    }
    Await.result(f, Duration(1, SECONDS))
    assert(in.closed == true)
    assert(source.isReleased == true)
    assert(sink.isReleased == true)
  }

  // PipeSource and PipeSink must release resources when interrupted during copy streams"
  @Test
  def runloopInterrupted() {
    val in = new DebugInfinityInputStream
    val (source, sink) = sourceSink()
    val out = new DebugOutputStream
    source connectIn in
    sink connectOut out
    val f = Future {
      source.ensureRunloopStarted()
      sink.ensureRunloopStarted()
      source.release()
      sink.release()
    }
    Await.result(f, Duration(1, SECONDS))
    assert(in.closed == true)
    assert(out.closed == true)
    assert(source.isReleased == true)
    assert(sink.isReleased == true)
  }
}
