
import scala.sys.process.debug._
import scala.sys.process.{ProcessLogger, BasicIO}
import java.io.{Closeable, ByteArrayInputStream, ByteArrayOutputStream, InputStream, IOException}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.control.Exception._
import scala.concurrent.ExecutionContext.Implicits.global

object Test extends App {
  // Each test normally ends in a moment, but for a worst case, 
  // waits until 5 seconds and breaks off.

  // Tests for PipedProcesses

  println("PipedProcesses need not to release resources when it normally ends")

  {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = false)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    Await.result(f, Duration(5, SECONDS))
    println(s"- source.releaseCount: ${source.releaseCount}")
    println(s"- sink.releaseCount: ${sink.releaseCount}")
    println(s"- a.destroyCount: ${a.destroyCount}")
    println(s"- b.destroyCount: ${b.destroyCount}")
  }

  println("PipedProcesses must release resources when b.run() fails")

  {
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
    Await.result(f, Duration(5, SECONDS))
    println(s"- source.releaseCount: ${source.releaseCount}")
    println(s"- sink.releaseCount: ${sink.releaseCount}")
    println(s"- a.destroyCount: ${a.destroyCount}")
    println(s"- b.destroyCount: ${b.destroyCount}")
  }

  println("PipedProcesses must release resources when a.run() fails")

  {
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
    Await.result(f, Duration(5, SECONDS))
    println(s"- source.releaseCount: ${source.releaseCount}")
    println(s"- sink.releaseCount: ${sink.releaseCount}")
    println(s"- a.destroyCount: ${a.destroyCount}")
    println(s"- b.destroyCount: ${b.destroyCount}")
  }

  println("PipedProcesses must release resources when interrupted waiting for first.exitValue()")

  {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = true)
    val b = new ProcessMock(error = false)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    Await.result(f, Duration(5, SECONDS))
    println(s"- source.releaseCount: ${source.releaseCount}")
    println(s"- sink.releaseCount: ${sink.releaseCount}")
    println(s"- a.destroyCount: ${a.destroyCount}")
    println(s"- b.destroyCount: ${b.destroyCount}")
  }

  println("PipedProcesses must release resources when interrupted waiting for second.exitValue()")

  {
    val io = BasicIO(false, ProcessLogger(_ => ()))
    val source = new PipeSourceMock
    val sink = new PipeSinkMock
    val a = new ProcessMock(error = false)
    val b = new ProcessMock(error = true)
    val p = new PipedProcesses(new ProcessBuilderMock(a, error = false), new ProcessBuilderMock(b, error = false), io, false)
    val f = Future {
      p.callRunAndExitValue(source, sink)
    }
    Await.result(f, Duration(5, SECONDS))
    println(s"- source.releaseCount: ${source.releaseCount}")
    println(s"- sink.releaseCount: ${sink.releaseCount}")
    println(s"- a.destroyCount: ${a.destroyCount}")
    println(s"- b.destroyCount: ${b.destroyCount}")
  }

  // Tests for PipeSource and PipeSink

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

  println("PipeSource and PipeSink must release resources when it normally ends")

  {
    val in = new DebugInputStream("aaa")
    val (source, sink) = sourceSink()
    val out = new DebugOutputStream
    source connectIn in
    sink connectOut out
    val f = Future {
      source.join()
      sink.join()
    }
    Await.result(f, Duration(5, SECONDS))
    println(s"- in.closed: ${in.closed}")
    println(s"- out.closed: ${out.closed}")
    println(s"- source.isReleased: ${source.isReleased}")
    println(s"- sink.isReleased: ${sink.isReleased}")
  }

  println("PipeSource and PipeSink must release resources when when waiting for source.take()")

  {
    val (source, sink) = sourceSink()
    val out = new DebugOutputStream
    sink connectOut out
    val f = Future {
      sink.ensureRunloopStarted()
      source.release()
      sink.release()
    }
    Await.result(f, Duration(5, SECONDS))
    println(s"- out.closed: ${out.closed}")
    println(s"- source.isReleased: ${source.isReleased}")
    println(s"- sink.isReleased: ${sink.isReleased}")
  }

  println("PipeSource and PipeSink must release resources when when waiting for sink.take()")

  {
    val in = new DebugInputStream("aaa")
    val (source, sink) = sourceSink()
    source connectIn in
    val f = Future {
      source.ensureRunloopStarted()
      source.release()
      sink.release()
    }
    Await.result(f, Duration(5, SECONDS))
    println(s"- in.closed: ${in.closed}")
    println(s"- source.isReleased: ${source.isReleased}")
    println(s"- sink.isReleased: ${sink.isReleased}")
  }

  println("PipeSource and PipeSink must release resources when copying stream")

  {
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
    Await.result(f, Duration(5, SECONDS))
    println(s"- in.closed: ${in.closed}")
    println(s"- out.closed: ${out.closed}")
    println(s"- source.isReleased: ${source.isReleased}")
    println(s"- sink.isReleased: ${sink.isReleased}")
  }
}

package scala.sys.process {
  import java.io._
  import java.lang.reflect.InvocationTargetException

  object debug {

    class ProcessMock(error: Boolean) extends Process {
      var destroyCount = 0
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
  }
}