import scala.concurrent.{ Await, Future, blocking }
import scala.concurrent.duration._

object Test {
  def main(args: Array[String]): Unit = {
      import scala.concurrent.ExecutionContext.Implicits._
      val time = 2000
      def spawn(nTasks: Int, name: String, useBlocking: Boolean): Future[Unit] = {
        def log(str: String): Unit = if (true) () else {
          val t = Thread.currentThread()
          println(s"${java.util.Calendar.getInstance().getTime()} ${name} (${t.getName}:${t}): $str")
        }
        Future.traverse(Vector.range(1, nTasks + 1))(
          result => Future {
            if (useBlocking) {
              blocking {
                Thread.sleep(time)
                log(result.toString)
              }
            } else {
              Thread.sleep(time)
              log(result.toString)
            }

            result
          }).map(_ => ())
      }
      Await.result(spawn(600, "stage1", true), Duration(s"${1 + (time.toLong * 600)}s"))
      Await.result(spawn(100, "stage2", false), Duration(s"${1 + (time.toLong * 100)}s"))
      println("Ticket 10484 has been addressed")
  }
}