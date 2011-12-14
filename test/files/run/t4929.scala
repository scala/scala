object Test extends App {
  import scala.util.parsing.json._

  val LIMIT = 2000
  val count = new java.util.concurrent.atomic.AtomicInteger(0)

  var error:Option[Throwable] = None

  (1 to 20) foreach { i =>
    (new Thread {
      override def run() {
        while (count.getAndIncrement() < LIMIT && !error.isDefined) {
          try {
            JSON.parseFull("""{"foo": [1,2,3,4]}""")
          } catch {
            case t: Throwable => error = Some(t)
          }
        }
      }
    }).start()

  }

  while (count.get() < LIMIT && !error.isDefined) {
    Thread.sleep(1)
  }

  error foreach { throw(_) }

  println("success")

}
