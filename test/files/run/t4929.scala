import scala.util.parsing.json._
import java.util.concurrent._
import collection.JavaConversions._

@deprecated("Suppress warnings", since="2.11")
object Test extends App {

  val LIMIT = 2000
  val THREAD_COUNT = 20
  val count = new java.util.concurrent.atomic.AtomicInteger(0)

  val begin = new CountDownLatch(THREAD_COUNT)
  val finish = new CountDownLatch(THREAD_COUNT)

  val errors = new ConcurrentLinkedQueue[Throwable]

  (1 to THREAD_COUNT) foreach { i =>
    val thread = new Thread {
      override def run() {
        begin.await(1, TimeUnit.SECONDS)
        try {
          while (count.getAndIncrement() < LIMIT && errors.isEmpty) {
            JSON.parseFull("""{"foo": [1,2,3,4]}""")
          }
        } catch {
          case t: Throwable => errors.add(t)
        }

        finish.await(10, TimeUnit.SECONDS)
      }
    }

    thread.setDaemon(true)
    thread.start()

  }


  errors foreach { throw(_) }

  println("success")

}
