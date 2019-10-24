package scala.build

import java.util.TimerTask
import java.io._
import java.util.concurrent.TimeUnit

// Prints some output to console periodically if the build itself is silent to avoid Travis CI's 10 minute timeout
// from aborting the build.
//
// A hopefully better behaved version of travis_wait and travis_wait_enhanced.
object TravisOutput {
  def install(): Unit = {
    val out = System.out
    if (out.getClass.getName != classOf[Redirecter].getName) {
      schedule()
      System.setOut(new Redirecter(out))
      savedOut = out
    }
  }

  def installIfOnTravis(): Unit =
    if (sys.env.contains("TRAVIS")) install()

  private var savedOut: PrintStream = _
  private val delayMS = TimeUnit.MINUTES.toMillis(9)
  private lazy val timer = new java.util.Timer(true)
  private var task: TimerTask = _

  private def schedule(): Unit = {
    task = new PrintNewline
    val period = delayMS
    timer.schedule(task, delayMS, period)
  }

  private def reschedule(): Unit = {
    task.cancel()
    schedule()
  }

  private class PrintNewline extends java.util.TimerTask() {
    override def run(): Unit = {
      savedOut.println("")
    }
  }

  private class Redirecter(stream: PrintStream) extends PrintStream(new OutputStream {
    def write(b: Int): Unit = {
      reschedule()
      stream.write(b)
    }
    override def write(b: Array[Byte]): Unit = {
      reschedule()
      stream.write(b)
    }
    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
      reschedule()
      stream.write(b, off, len)
    }
    override def flush(): Unit = {
      reschedule()
      stream.flush()
    }
    override def close(): Unit = {
      reschedule()
      stream.close()
    }
  })

}
