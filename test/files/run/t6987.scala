import java.io._
import tools.nsc.{CompileClient, CompileServer}

object Test extends App {
  // we have to explicitly launch our server because when the client launches a server it uses 
  // the "scala" shell command meaning whatever version of scala (and whatever version of libraries)
  // happens to be in the path gets used
  val t = new Thread(new Runnable {
    def run() = {
      CompileServer.main(Array[String]())
    }
  })
  t setDaemon true
  t.start()

  val baos = new ByteArrayOutputStream()
  val ps = new PrintStream(baos)

  val oldOut = System.out
 
  System.setOut(ps)

  try {
    // shut down the server via the client using the verbose flag
    val success = CompileClient.process(Array("-shutdown", "-verbose"))

    // now make sure we got success and a verbose result
    val msg = baos.toString()
    if (success) {
      if (msg contains "Settings after normalizing paths") {
        oldOut println "got successful verbose results!"
      } else {
        oldOut println "did not get the string expected, full results were:"
        oldOut println msg
      }
    } else {
      oldOut println "got a failure. Full results were:"
      oldOut println msg
    }
  } catch {
     case e : Throwable => 
         oldOut println e
  } finally {
    System setOut oldOut
  }
}
