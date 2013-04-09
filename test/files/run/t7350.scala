import scala.sys.process._
import scala.collection.JavaConversions._
import java.io.{InputStream, OutputStream}

class DummyOutput extends OutputStream {
  def write(b: Int) = ()
}

class DummyInput extends InputStream {
  def read(): Int = -1
}

object Test extends App {
  def procrastinate = {
    Thread.sleep(1000L)
    0
  }
  
  def processThreads = Thread.getAllStackTraces().filter {case (thread, trace) =>
    trace.count {el =>
      el.getClassName() contains "ProcessImpl"
    } != 0
  }
  
  // Count the number of processes running ProcessImpl code
  def processThreadCount = processThreads.size

  val oldThreadCount = processThreadCount
  println("Before thread count: " + oldThreadCount)

  val process = (Process("Procrastinator", procrastinate) #< new DummyInput #> new DummyOutput).run()
                                                  
  // Uncomment this line, and the problem disappears
  process.destroy()

  try {
    process.exitValue
    // This should be unreachable
  } catch {
    case e: RuntimeException => //This is normal - it shouldn't exit cleanly
  }

  // Give it a little extra time to clean up
  Thread.sleep(5000L)

  val newThreadCount = processThreadCount
  println("After thread count: " + newThreadCount)
  if (oldThreadCount != newThreadCount) {
    println("Threads still active")
    for ((thread, trace) <- processThreads) {
      println("Thread " + thread.toString + " still running")
      for (row <- trace) {
          println("  " + row.toString)
      }
    }
  }
}
