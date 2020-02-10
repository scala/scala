package scala.sys.process

import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.junit.Test
import java.io.{InputStream, OutputStream, PipedInputStream, PipedOutputStream, ByteArrayInputStream,
  ByteArrayOutputStream, IOException, Closeable}
import java.lang.reflect.InvocationTargetException
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.Exception.ignoring
import org.junit.Assert._

import scala.tools.testkit.AssertUtil.{readyOrNot, waitForIt}
import scala.tools.testkit.TestDuration

@RunWith(classOf[JUnit4])
class PipedProcessTest {

  // PipedProcesses need not to release resources when it normally end
  @Test
  def normallyEnd(): Unit = {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcessesMock(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future(p.callRunAndExitValue(source, sink))
    waitForIt(readyOrNot(f), label = "normallyEnd")
    assertEquals(Some(0), Await.result(f, TestDuration.Standard))
    assertEquals(0, source.releaseCount)
    assertEquals(0, sink.releaseCount)
    assertEquals(0, a.destroyCount)
    assertEquals(0, b.destroyCount)
  }

  @Test
  def shouldSyncRunAndExitValue(): Unit = {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock {
      override def runloop(src: InputStream, dst: OutputStream) = Thread.sleep(5) //used to simulate the block
    }
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcessesMock(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false) {
      override def newSource = source
      override def newSink   = sink
    }

    //p.exitValue()
    p.callRunAndExitValue(source, sink)

    assertFalse("Source is alive", source.isAlive)
  }

  // PipedProcesses must release resources when b.run() failed
  @Test
  def bFailed(): Unit = {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcessesMock(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = true), io, false)
    val f = Future {
      ignoring(classOf[IOException]) {
        p.callRunAndExitValue(source, sink): Unit
      }
    }
    waitForIt(f.isCompleted)
    assertEquals(1, source.releaseCount)
    assertEquals(1, sink.releaseCount)
    assertEquals(0, a.destroyCount)
    assertEquals(0, b.destroyCount)
  }

  // PipedProcesses must release resources when a.run() failed
  @Test
  def aFailed(): Unit = {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcessesMock(new ProcessBuilderMock(a, error = true), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      ignoring(classOf[IOException]) {
        p.callRunAndExitValue(source, sink): Unit
      }
    }
    waitForIt(f.isCompleted)
    assertEquals(1, source.releaseCount)
    assertEquals(1, sink.releaseCount)
    assertEquals(0, a.destroyCount)
    assertEquals(1, b.destroyCount)
  }

  // PipedProcesses must release resources when interrupted during waiting for first.exitValue()
  @Test
  def firstInterrupted(): Unit = {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = true)
    val b = new ProcessMock(error = false)
    val p = new PipedProcessesMock(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    waitForIt(f.isCompleted)
    assertEquals(1, source.releaseCount)
    assertEquals(1, sink.releaseCount)
    assertEquals(1, a.destroyCount)
    assertEquals(1, b.destroyCount)
  }

  // PipedProcesses must release resources when interrupted during waiting for second.exitValue()
  @Test
  def secondInterrupted(): Unit = {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = true)
    val p = new PipedProcessesMock(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    waitForIt(f.isCompleted)
    assertEquals(1, source.releaseCount)
    assertEquals(1, sink.releaseCount)
    assertEquals(1, a.destroyCount)
    assertEquals(1, b.destroyCount)
  }
}

@RunWith(classOf[JUnit4])
class PipeSourceSinkTest {
  def throwsIOException(f: => Unit) = {
    try { f; false }
    catch { case _: IOException => true }
  }

  class PipeSink extends Process.PipeSink("TestPipeSink") {
    def ensureRunloopStarted() = while (sink.isSet) Thread.sleep(10L)
    def isReleased = {
      val field = classOf[Process.PipeSink].getDeclaredField("pipe")
      field.setAccessible(true)
      val pipe = field.get(this).asInstanceOf[PipedInputStream]
      !this.isAlive && throwsIOException { pipe.read() }
    }
  }

  class PipeSource extends Process.PipeSource("TestPipeSource") {
    def ensureRunloopStarted() = while (source.isSet) Thread.sleep(10L)
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
  def normallyEnd(): Unit = {
    val in = new DebugInputStream("aaa")
    val (source, sink) = sourceSink()
    val out = new DebugOutputStream
    source connectIn in
    sink connectOut out
    val f = Future {
      source.done()
      source.join()
      sink.done()
      sink.join()
    }
    waitForIt(f.isCompleted)
    assertTrue(in.closed)
    assertTrue(out.closed)
    assertTrue(source.isReleased)
    assertTrue(sink.isReleased)
  }

  // PipeSource and PipeSink must release resources when interrupted during waiting for source.take()
  @Test
  def sourceInterrupted(): Unit = {
    val (source, sink) = sourceSink()
    val out = new DebugOutputStream
    sink connectOut out
    val f = Future {
      sink.ensureRunloopStarted()
      source.release()
      sink.release()
    }
    waitForIt(f.isCompleted)
    assertTrue(out.closed)
    assertTrue(source.isReleased)
    assertTrue(sink.isReleased)
  }

  // PipeSource and PipeSink must release resources when interrupted during waiting for sink.take()
  @Test
  def sinkInterrupted(): Unit = {
    val in = new DebugInputStream("aaa")
    val (source, sink) = sourceSink()
    source connectIn in
    val f = Future {
      source.ensureRunloopStarted()
      source.release()
      sink.release()
    }
    waitForIt(f.isCompleted)
    assertTrue(in.closed)
    assertTrue(source.isReleased)
    assertTrue(sink.isReleased)
  }

  // PipeSource and PipeSink must release resources when interrupted during copy streams
  @Test
  def runloopInterrupted(): Unit = {
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
    waitForIt(f.isCompleted)
    assertTrue(in.closed)
    assertTrue(out.closed)
    assertTrue(source.isReleased)
    assertTrue(sink.isReleased)
  }
}

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
  // no work to do
  override def runloop(src: InputStream, dst: OutputStream) = BasicIO close dst
  override def release(): Unit = { releaseCount += 1 ; super.release() }
}

class PipeSourceMock extends Process.PipeSource("PipeSourceMock") {
  var releaseCount = 0
  // no work to do
  override def runloop(src: InputStream, dst: OutputStream) = BasicIO close src
  override def release(): Unit = { releaseCount += 1 ; super.release() }
}

class PipedProcessesMock(a: ProcessBuilder, b: ProcessBuilder, defaultIO: ProcessIO, toError: Boolean)
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
