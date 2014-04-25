// test concurrent calls to Stream.tail
@deprecated("Suppress warnings", since="2.11")
object Test  {

  def slowRange(from: Int, until: Int, cons: (Int, => Stream[Int]) => Stream[Int]): Stream[Int] = {
    var current = from
    def next: Stream[Int] = {
      Thread.sleep(100)
      if (current >= until) Stream.empty
      else {
        val stream = cons(current, next)
        current += 1
        stream
      }
    }
    next
  }

  def testCons(cons: (Int, => Stream[Int]) => Stream[Int]): Unit = {

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val stream = slowRange(0, 10, cons)

    val fut0 = Future(stream.toList)
    val fut1 = Future(stream.toList)

    val res0 = Await.result(fut0, 1.second)
    val res1 = Await.result(fut1, 1.second)

    println("Evaluation 0: " + res0)
    println("Evaluation 1: " + res1)
  }

  def main(args: Array[String]) {
    println("Testing standard cons.")
    testCons(Stream.cons.apply(_, _))
  }
}

