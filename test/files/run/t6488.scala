//> using javaOpt -Dforked.test=yes.please

import scala.sys.process._
import scala.util.Try
import scala.util.Properties.{javaHome, javaClassPath, userDir}
import java.io.{File, IOException}, File.pathSeparator
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit._
import java.util.concurrent.atomic._
import scala.reflect.io.Path

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
  def main(args: Array[String]): Unit = {
    if (args.nonEmpty && args(0) == "data")
      data()
    else
      test()          // args(0) == "jvm"
  }

  def java(): File =
    new File(javaHome, "bin").listFiles.sorted.filter(f => Path(f).stripExtension == "java").find(_.canExecute).getOrElse {
      // todo signal test runner that test is skipped
      new File("/bin/ls")  // innocuous
    }

  // fork the data spewer, wait for input, then destroy the process
  def test(): Unit = {
    //Process(f.getAbsolutePath).run(ProcessLogger { _ => () }).destroy
    val reading = new CountDownLatch(1)
    val count   = new AtomicInteger
    def counted = count.get
    // when run in-process, outdir is not absolute path; also, outdir is not listable for some reason.
    //val outdir  = s"$userDir/test/files/run/${System.getProperty("partest.output")}"
    val outdir  = System.getProperty("partest.output")
    val command = java().getAbsolutePath ::
                  "Test" ::
                  "data" ::
                  Nil
                  // re-adding outdir to classpath only required for in-process exec, which is broken
                  //"-classpath" ::
                  //s"${javaClassPath}${pathSeparator}${outdir}" ::
    Try {
      Process(command).run(ProcessLogger { (s: String) =>
        //Console println s"[[$s]]"     // java help
        count.getAndIncrement
        reading.countDown
        Thread.`yield`()
      })
    } foreach { (p: Process) =>
      val ok = reading.await(10, SECONDS)
      if (!ok) Console println "Timed out waiting for process output!"
      p.destroy()
    }
    //Console println s"Read count $counted lines"
  }

  // spew something
  def data(): Unit = {
    def filler = "." * 100
    for (i <- 1 to 1000)
      Console println s"Outputting data line $i $filler"
  }
}
