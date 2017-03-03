import scala.concurrent._, duration._
import ExecutionContext.Implicits.global
import scala.tools.reflect.WrappedProperties.AccessControl._
import java.util.concurrent.CyclicBarrier

object Test extends App {
  @volatile var done = false
  val barrier = new CyclicBarrier(2)

  val probe = Future {
    val attempts     = 1024          // previously, failed after a few
    def fail(i: Int) = s"Failed at $i"
    barrier.await()
    for (i <- 1 to attempts ; p <- systemProperties)
      p match { case (k, v) => assert (k != null && v != null, fail(i)) }
  }
  probe onComplete {
    case _ => done = true
  }

  System.setProperty("foo", "fooz")
  System.setProperty("bar", "barz")
  barrier.await()   // just for fun, wait to start mucking with properties

  // continually modify properties trying to break live iteration over sys props
  // hint: don't iterate lively over sys props
  var alt = true
  while (!done) {
    if (alt) {
      System.getProperties.remove("foo")
      System.setProperty("bar", "barz")
      alt = false
    } else {
      System.getProperties.remove("bar")
      System.setProperty("foo", "fooz")
      alt = true
    }
  }
  Await.result(probe, Duration.Inf)
}

/*
import scala.concurrent.{duration, Future, Await, ExecutionContext}
import scala.tools.nsc.Settings
import ExecutionContext.Implicits.global

// Was failing pretty regularly with a ConcurrentModificationException as
// WrappedProperties#systemProperties iterated directly over the mutable
// global system properties map.
object Test {
  def main(args: Array[String]) {
    val tries = 1000 // YMMV
    val compiler = Future {
      for(_ <- 1 to tries) new Settings(_ => {})
    }
    for(i <- 1 to tries * 10) System.setProperty(s"foo$i", i.toString)
    Await.result(compiler, duration.Duration.Inf)
  }
}
*/
