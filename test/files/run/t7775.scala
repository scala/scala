import scala.concurrent.{duration, future, Await, ExecutionContext}
import scala.tools.nsc.Settings
import ExecutionContext.Implicits.global

// Was failing pretty regularly with a ConcurrentModificationException as
// WrappedProperties#systemProperties iterated directly over the mutable
// global system properties map.
object Test {
  def main(args: Array[String]) {
    val tries = 1000 // YMMV
    val compiler = future {
      for(_ <- 1 to tries) new Settings(_ => {})
    }
    for(i <- 1 to tries * 10) System.setProperty(s"foo$i", i.toString)
    Await.result(compiler, duration.Duration.Inf)
  }
}
