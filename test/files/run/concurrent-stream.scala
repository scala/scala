// test concurrent calls to Stream.tail
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
  import scala.actors.Actor._

  val stream = slowRange(0, 10, cons)
  val main = self
  actor { main ! stream.toList }
  actor { main ! stream.toList }
  val eval0 = receive { case list: List[Int] => list }
  val eval1 = receive { case list: List[Int] => list }
  println("Evaluation 0: " + eval0)
  println("Evaluation 1: " + eval1)
}

  def main(args: Array[String]) {
    println("Testing standard cons.")
    testCons(Stream.cons.apply(_, _))
  }
}

