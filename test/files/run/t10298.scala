import collection.immutable._

object Test {

  def main(args: Array[String]): Unit = {
    assert((Queue(1) ++ Vector(2)) == Queue(1, 2))

    assert(((Queue(1).++(Vector(2))(collection.breakOut)): Vector[Int]) == Vector(1, 2))

    assert(((Queue(1) :+ 2) ++ Vector(3)) == Queue(1, 2, 3))

    assert(((1 +: Queue(2)) ++ Vector(3)) == Queue(1, 2, 3))

    assert(((1 +: Queue(2)) ++ (3 +: Queue(4))) == Queue(1, 2, 3, 4))
  }

}
