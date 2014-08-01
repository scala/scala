import scala.sys.process._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.io.ByteArrayInputStream

object Test {
  def main(args: Array[String]) {
    def input = new ByteArrayInputStream(("a").getBytes())
    def process =
      if (System.getProperty("os.name", "") startsWith "Windows")
        "find /v \"\"" #< input
      else
        "wc" #< input
    val f = Future {
      for (i <- 0 until 10) {
        process.run(ProcessLogger { _ => () }).exitValue()
      }
    }
    Thread.sleep(1000)
    println(f.isCompleted)
  }
}

