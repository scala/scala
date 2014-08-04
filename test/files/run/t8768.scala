import scala.sys.process.{Process, ProcessLogger}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.ByteArrayInputStream

object Test {
  // This test normally ends within a second, but for a worst case, 
  // waits until 60 seconds and breaks off if Process.exitValue() blocks.
  def main(args: Array[String]) {
    def input = new ByteArrayInputStream(("a").getBytes())
    def process =
      if (System.getProperty("os.name", "") startsWith "Windows")
        Process("find /v \"\"") #< input
      else
        Process("wc") #< input
    val f = Future {
      for (i <- 0 until 100) {
        process.run(ProcessLogger { _ => () }).exitValue()
      }
    }
    Await.result(f, Duration(60, SECONDS))
    println("OK")
  }
}

