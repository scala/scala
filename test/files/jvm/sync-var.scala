import java.util.concurrent._
import java.util.concurrent.atomic._

object Test { def main(args: Array[String]) {

val n = 10000
val i = new AtomicInteger(n)
val j = new AtomicInteger(n)
val sum = new AtomicInteger

val q = new scala.concurrent.SyncVar[Int]

val producers = (1 to 3) map { z => new Thread {
  override def run() {
    var again = true
    while (again) {
      val x = i.getAndDecrement()
      if (x > 0)
        q put x
      else
        again = false
    }
  }
} }

val summers = (1 to 7) map { z => new Thread {
  override def run() {
    val x = j.decrementAndGet()
    if (x >= 0) {
      sum addAndGet q.take()
    }
    if (x > 0) {
      run()
    } else {
      // done
    }
  }
} }

summers foreach { _.start() }
producers foreach { _.start() }

summers foreach { _.join() }

val got = sum.get
val expected = (n + 1) * n / 2
println(got + " " + expected + " " + (got == expected))

producers foreach { _.join() }

} }

// vim: set ts=2 sw=2 et:
