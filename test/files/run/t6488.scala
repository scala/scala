import scala.sys.process._
import scala.util.Try
import scala.util.Properties.{ javaHome, javaClassPath }
import java.io.{ File, IOException }
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit._
import java.util.concurrent.atomic._

object Test {
  /*
  // Program that prints "Success" if the command was successfully run then destroyed
  // It will silently pass if the command "/bin/ls" does not exist
  // It will fail due to the uncatchable exception in t6488 race condition,
  // i.e., if any uncaught exceptions on spawned threads are printed.
  def main(args: Array[String]) {
    try Process("/bin/ls").run(ProcessLogger { _ => () }).destroy
    catch { case _ => () }
    println("Success")
  }
  */

  // Show that no uncaught exceptions are thrown on spawned I/O threads
  // when the process is destroyed.  The default handler will print
  // stack traces in the failing case.
  def main(args: Array[String]) {
    if (args.nonEmpty && args(0) == "data")
      data()
    else
      test()          // args(0) == "jvm"
  }

  // fork the data spewer, wait for input, then destroy the process
  def test() {
    val f = new File(javaHome, "bin").listFiles.sorted filter (_.getName startsWith "java") find (_.canExecute) getOrElse {
      // todo signal test runner that test is skipped
      new File("/bin/ls")  // innocuous
    }
    //Process(f.getAbsolutePath).run(ProcessLogger { _ => () }).destroy
    val reading = new CountDownLatch(1)
    val count   = new AtomicInteger
    def counted = count.get
    val command = s"${f.getAbsolutePath} -classpath ${javaClassPath} Test data"
    Try {
      Process(command) run ProcessLogger { (s: String) =>
        //Console println s"[[$s]]"     // java help
        count.getAndIncrement
        reading.countDown
        Thread.`yield`()
      }
    } foreach { (p: Process) =>
      val ok = reading.await(10, SECONDS)
      if (!ok) Console println "Timed out waiting for process output!"
      p.destroy()
    }
    //Console println s"Read count $counted lines"
  }

  // spew something
  def data() {
    def filler = "." * 100
    for (i <- 1 to 1000)
      Console println s"Outputting data line $i $filler"
  }
}
