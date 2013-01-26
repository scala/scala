import java.io._
import tools.nsc.{CompileClient, CompileServer}
import java.util.concurrent.{CountDownLatch, TimeUnit}

object Test extends App {
  val startupLatch = new CountDownLatch(1)
  // we have to explicitly launch our server because when the client launches a server it uses 
  // the "scala" shell command meaning whatever version of scala (and whatever version of libraries)
  // happens to be in the path gets used
  val t = new Thread(new Runnable {
    def run() = {
      CompileServer.execute(() => startupLatch.countDown(), Array[String]())
    }
  })
  t setDaemon true
  t.start()
  if (!startupLatch.await(2, TimeUnit.MINUTES))
    sys error "Timeout waiting for server to start"

  val baos = new ByteArrayOutputStream()
  val ps = new PrintStream(baos) 

  val success = (scala.Console withOut ps) {
    // shut down the server via the client using the verbose flag
    CompileClient.process(Array("-shutdown", "-verbose"))
  }

  // now make sure we got success and a verbose result
  val msg = baos.toString()

  if (success) {
    if (msg contains "Settings after normalizing paths") {
     println("got successful verbose results!")
    } else {
     println("did not get the string expected, full results were:")
     println(msg)
    }
  } else {
    println("got a failure. Full results were:")
    println(msg)
  }
  scala.Console.flush
}
