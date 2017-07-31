import scala.collection.immutable._

object Test {

  def main(args: Array[String]): Unit = {
    val inputs: List[(Queue[Int], Vector[Int])] = List(
      Queue.empty -> Vector(0, 1, 2),
      (Queue.empty :+ 0) -> Vector(1, 2),
      (0 +: Queue.empty) -> Vector(1, 2),
      (0 +: (Queue.empty :+ 1)) -> Vector(2),
      ((0 +: Queue.empty) :+ 1) -> Vector(2),
      (0 +: 1 +: Queue.empty) -> Vector(2),
      (Queue.empty :+ 0 :+ 1) -> Vector(2)
    )

    inputs.foreach { case (q, v) => assert(q ++ v == Queue(0, 1, 2)) }
  }

}
